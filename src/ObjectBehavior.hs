module ObjectBehavior (aicube, camera) where

import Maybe (fromJust)

import AFRP
import Camera
import IdentityList
import MD3
import Matrix
import Object
import Parser

ray ::
    (Double, Double, Double) ->
      (Double, Double, Double) -> ILKey -> ILKey -> Object
ray (x, y, z) (vx, vy, vz) firedfrom iD
  = (arr
       (\ oi ->
          let clippedPos = oiCollisionPos oi in
            let grounded = oiOnLand oi in ((), (clippedPos, grounded)))
       >>>
       (first (after 0.25 ()) >>>
          arr
            (\ (timeout, (clippedPos, grounded)) ->
               (grounded, (clippedPos, timeout))))
         >>>
         (first ((iPre False) <<< identity) >>>
            arr
              (\ (cl, (clippedPos, timeout)) ->
                 let (vvx, vvy, vvz) = normalise $ vectorSub end start in
                   (clippedPos, (cl, timeout, vvx, vvy, vvz))))
           >>>
           (first ((iPre start) <<< (arr not0)) >>>
              arr
                (\ (clip, (cl, timeout, vvx, vvy, vvz)) ->
                   (vvx, (cl, clip, timeout, vvy, vvz))))
             >>>
             (first (arr (\ vvx -> 7500 * vvx) >>> integral) >>>
                arr
                  (\ (ucx, (cl, clip, timeout, vvy, vvz)) ->
                     (vvy, (cl, clip, timeout, ucx, vvz))))
               >>>
               (first (arr (\ vvy -> 7500 * vvy) >>> integral) >>>
                  arr
                    (\ (ucy, (cl, clip, timeout, ucx, vvz)) ->
                       (vvz, (cl, clip, timeout, ucx, ucy))))
                 >>>
                 (first (arr (\ vvz -> 7500 * vvz) >>> integral) >>>
                    arr
                      (\ (ucz, (cl, clip, timeout, ucx, ucy)) ->
                         (cl, (cl, clip, timeout, ucx, ucy, ucz))))
                   >>>
                   (first edge >>>
                      arr
                        (\ (clipev, (cl, clip, timeout, ucx, ucy, ucz)) ->
                           ObjOutput{ooObsObjState =
                                       OOSRay{rayStart = start, rayEnd = clip,
                                              rayUC = vectorAdd start (ucx, ucy, ucz), clipped = cl,
                                              firedFrom = firedfrom},
                                     ooKillReq = timeout, ooSpawnReq = noEvent,
                                     ooSendMessage =
                                       clipev `tag` [(firedfrom, (iD, Coord clip))]})))
  where (start, end) = firePos (x, y, z) (vx, vy, vz)
        (_, _, _) = normalise $ vectorSub end start
        not0 c
          | c /= (0, 0, 0) = c
          | otherwise = (x, y, z)

projectile :: (Vec3, Vec3) -> ILKey -> ILKey -> Object
projectile ((sx, sy, sz), (vx, vy, vz)) firedfrom _
  = (arr
       (\ oi ->
            let grounded = oiOnLand oi in
              let hits = oiHit oi in (hits, grounded))
       >>>
       (first identity >>> arr (\ (hit, grounded) -> (grounded, hit))) >>>
         ((first ((iPre False) <<< identity) >>> first edge) >>>
            arr (\ (clipEv, hit) -> ((), (clipEv, hit))))
           >>>
           (first (arr (\ () -> 1500 * vx) >>> imIntegral sx) >>>
              arr (\ (x, (clipEv, hit)) -> ((), (clipEv, hit, x))))
             >>>
             (first (arr (\ () -> 1500 * vy) >>> imIntegral sy) >>>
                arr (\ (y, (clipEv, hit, x)) -> ((), (clipEv, hit, x, y))))
               >>>
               (first (arr (\ () -> 1500 * vz) >>> imIntegral sz) >>>
                  arr
                    (\ (z, (clipEv, hit, x, y)) ->
                       ((x, y, z), (clipEv, hit, x, y, z))))
                 >>>
                 (first (iPre (sx, sy, sz) <<< identity) >>>
                    arr
                      (\ (oldpos, (clipEv, hit, x, y, z)) ->
                         ((clipEv, hit), (oldpos, x, y, z))))
                   >>>
                   (first
                      (arr (\ (clipEv, hit) -> (isEvent clipEv || isEvent hit)) >>> edge)
                      >>>
                      arr
                        (\ (hitEv, (oldpos, x, y, z)) ->
                           ObjOutput{ooObsObjState =
                                       OOSProjectile{projectileOldPos = oldpos,
                                                     projectileNewPos = (x, y, z),
                                                     firedFrom = firedfrom},
                                     ooKillReq = hitEv, ooSpawnReq = noEvent,
                                     ooSendMessage = noEvent})))

camera ::
       Camera ->
         [(String, AnimState, AnimState)] ->
           [(ILKey, Message)] -> ILKey -> Object
camera cam _ _ iD
  = (arr
       (\ oi ->
          let gi = oiGameInput oi in
            let clippedcam = oiCollision oi in
              let grounded = oiOnLand oi in
                let msgs = oiMessage oi in
                  (gi, (clippedcam, gi, grounded, msgs, oi)))
       >>>
       (first ptrPos >>>
          arr
            (\ (pPos, (clippedcam, gi, grounded, msgs, oi)) ->
               (gi, (clippedcam, gi, grounded, msgs, oi, pPos))))
         >>>
         (first (movementKS 400) >>>
            arr
              (\ (forwardVel, (clippedcam, gi, grounded, msgs, oi, pPos)) ->
                 (gi, (clippedcam, forwardVel, gi, grounded, msgs, oi, pPos))))
           >>>
           (first (strafeKS 400) >>>
              arr
                (\ (strafeVel,
                    (clippedcam, forwardVel, gi, grounded, msgs, oi, pPos))
                   ->
                   (gi,
                    (clippedcam, forwardVel, gi, grounded, msgs, oi, pPos,
                     strafeVel))))
             >>>
             (first lbp >>>
                arr
                  (\ (trigger,
                      (clippedcam, forwardVel, gi, grounded, msgs, oi, pPos, strafeVel))
                     ->
                     (gi,
                      (clippedcam, forwardVel, gi, grounded, msgs, oi, pPos, strafeVel,
                       trigger))))
               >>>
               (first rbp >>>
                  arr
                    (\ (rtrigger,
                        (clippedcam, forwardVel, gi, grounded, msgs, oi, pPos, strafeVel,
                         trigger))
                       ->
                       (gi,
                        (clippedcam, forwardVel, gi, grounded, msgs, oi, pPos, rtrigger,
                         strafeVel, trigger))))
                 >>>
                 (first getDt >>>
                    arr
                      (\ (dt,
                          (clippedcam, forwardVel, gi, grounded, msgs, oi, pPos, rtrigger,
                           strafeVel, trigger))
                         ->
                         ((clippedcam, pPos),
                          (clippedcam, dt, forwardVel, gi, grounded, msgs, oi, rtrigger,
                           strafeVel, trigger))))
                   >>>
                   (first
                      (arr (\ (clippedcam, pPos) -> (pPos, clippedcam)) >>>
                         ((iPre cam) <<< (arr setView)))
                      >>>
                      arr
                        (\ (cam1,
                            (clippedcam, dt, forwardVel, gi, grounded, msgs, oi, rtrigger,
                             strafeVel, trigger))
                           ->
                           ((cam1, dt, forwardVel),
                            (cam1, clippedcam, dt, gi, grounded, msgs, oi, rtrigger, strafeVel,
                             trigger))))
                     >>>
                     (first
                        (arr (\ (cam1, dt, forwardVel) -> (forwardVel * dt, cam1)) >>>
                           moves)
                        >>>
                        arr
                          (\ (cam2,
                              (cam1, clippedcam, dt, gi, grounded, msgs, oi, rtrigger, strafeVel,
                               trigger))
                             ->
                             ((cam2, dt, strafeVel),
                              (cam1, clippedcam, gi, grounded, msgs, oi, rtrigger, trigger))))
                       >>>
                       (first
                          (arr (\ (cam2, dt, strafeVel) -> (strafeVel * dt, cam2)) >>>
                             strafes)
                          >>>
                          arr
                            (\ (cam3,
                                (cam1, clippedcam, gi, grounded, msgs, oi, rtrigger, trigger))
                               ->
                               ((gi, grounded),
                                (cam1, cam3, clippedcam, msgs, oi, rtrigger, trigger))))
                         >>>
                         (first (arr (\ (gi, grounded) -> (grounded, gi)) >>> fallingp) >>>
                            arr
                              (\ (yVel, (cam1, cam3, clippedcam, msgs, oi, rtrigger, trigger)) ->
                                 ((cam3, yVel), (cam1, clippedcam, msgs, oi, rtrigger, trigger))))
                           >>>
                           ((first (arr dropCam) >>>
                               loop
                                 (arr
                                    (\ ((cam4, (cam1, clippedcam, msgs, oi, rtrigger, trigger)),
                                        msgn)
                                       ->
                                       ((msgn, msgs, rtrigger),
                                        (cam1, cam4, clippedcam, msgs, oi, rtrigger, trigger)))
                                    >>>
                                    (first
                                       (arr
                                          (\ (msgn, msgs, rtrigger) ->
                                             case (isEvent rtrigger) of
                                                 True -> ([], msgn)
                                                 False -> ((getMsg0 msgs msgn), msgn))
                                          >>> ((iPre ([], [])) <<< identity))
                                       >>>
                                       arr
                                         (\ ((msgn, msgi),
                                             (cam1, cam4, clippedcam, msgs, oi, rtrigger, trigger))
                                            ->
                                            ((cam1, cam4, clippedcam, msgi, msgs, oi, rtrigger,
                                              trigger),
                                             msgn)))))
                              >>>
                              arr
                                (\ (cam1, cam4, clippedcam, msgi, msgs, oi, rtrigger, trigger) ->
                                   (oi, (cam1, cam4, clippedcam, msgi, msgs, rtrigger, trigger))))
                             >>>
                             ((first (arr (\ oi -> oiHit oi) >>> (iPre (noEvent) <<< identity))
                                 >>>
                                 loop
                                   (arr
                                      (\ ((hitEv,
                                           (cam1, cam4, clippedcam, msgi, msgs, rtrigger, trigger)),
                                          currentHealth)
                                         ->
                                         ((currentHealth, hitEv),
                                          (cam1, cam4, clippedcam, msgi, msgs, rtrigger, trigger)))
                                      >>>
                                      (first
                                         (arr
                                            (\ (currentHealth, hitEv) ->
                                               case (isEvent hitEv) of
                                                   True -> currentHealth -
                                                             (realToFrac
                                                                ((length (fromEvent hitEv)) * 3))
                                                   False -> currentHealth)
                                            >>> ((iPre 100) <<< identity))
                                         >>>
                                         arr
                                           (\ (currentHealth,
                                               (cam1, cam4, clippedcam, msgi, msgs, rtrigger,
                                                trigger))
                                              ->
                                              ((cam1, cam4, clippedcam, currentHealth, msgi, msgs,
                                                rtrigger, trigger),
                                               currentHealth)))))
                                >>>
                                arr
                                  (\ (cam1, cam4, clippedcam, currentHealth, msgi, msgs, rtrigger,
                                      trigger)
                                     ->
                                         (msgs,
                                          (cam1, cam4, clippedcam, currentHealth, msgi, rtrigger,
                                           trigger))))
                               >>>
                               ((first ((iPre noEvent) <<< identity) >>>
                                   loop
                                     (arr
                                        (\ ((msges,
                                             (cam1, cam4, clippedcam, currentHealth, msgi, rtrigger,
                                              trigger)),
                                            kills)
                                           ->
                                           ((kills, msges),
                                            (cam1, cam4, clippedcam, currentHealth, msges, msgi,
                                             rtrigger, trigger)))
                                        >>>
                                        (first
                                           (arr
                                              (\ (kills, msges) ->
                                                 kills + (length (findKills (event2List msges))))
                                              >>> ((iPre 0) <<< identity))
                                           >>>
                                           arr
                                             (\ (kills,
                                                 (cam1, cam4, clippedcam, currentHealth, msges,
                                                  msgi, rtrigger, trigger))
                                                ->
                                                ((cam1, cam4, clippedcam, currentHealth, kills,
                                                  msges, msgi, rtrigger, trigger),
                                                 kills)))))
                                  >>>
                                  arr
                                    (\ (cam1, cam4, clippedcam, currentHealth, kills, msges, msgi,
                                        rtrigger, trigger)
                                       ->
                                       ((msgi, rtrigger),
                                        (cam1, cam4, clippedcam, currentHealth, kills, msges,
                                         trigger))))
                                 >>>
                                 (first
                                    (arr (\ (msgi, rtrigger) -> (rtrigger, msgi)) >>>
                                       ((iPre (noEvent, [])) <<< identity))
                                    >>>
                                    arr
                                      (\ ((rev, msgi2),
                                          (cam1, cam4, clippedcam, currentHealth, kills, msges,
                                           trigger))
                                         ->
                                         (clippedcam,
                                          (cam1, cam4, currentHealth, kills, msges, msgi2, rev,
                                           trigger))))
                                   >>>
                                   (first ((iPre cam) <<< identity) >>>
                                      arr
                                        (\ (ccam,
                                            (cam1, cam4, currentHealth, kills, msges, msgi2, rev,
                                             trigger))
                                           ->
                                           ObjOutput{ooSpawnReq =
                                                       (trigger `tag`
                                                          [(ray (cpos cam1) (viewPos cam1) iD)]),
                                                     ooObsObjState =
                                                       OOSCamera{newCam = cam4, oldCam = cam1,
                                                                 health = currentHealth, ammo = 100,
                                                                 score = kills,
                                                                 cood =
                                                                   case (isEvent rev) of
                                                                       True -> reverse $
                                                                                 map getCoordFromMsg
                                                                                   (msgi2)
                                                                       _ -> []},
                                                     ooKillReq = noEvent,
                                                     ooSendMessage =
                                                       case (event2List msges) of
                                                           [] -> noEvent
                                                           _ -> (Event ()) `tag`
                                                                  (case
                                                                     (findEnemies
                                                                        (event2List msges))
                                                                     of
                                                                       [] -> []
                                                                       _ -> [toTargetPosition iD
                                                                               (cpos ccam)
                                                                               (head
                                                                                  (findEnemies
                                                                                     (event2List
                                                                                        msges)))])})))

event2List :: Event [a] -> [a]
event2List ev
  | isEvent ev = fromEvent ev
  | otherwise = []

getMsg0 ::
        Event [(ILKey, Message)] ->
          [(ILKey, Message)] -> [(ILKey, Message)]
getMsg0 (ev) ls
  = case (isEvent ev) of
        True -> case ((findCoords (fromEvent ev)) ++ ls) of
                    x -> x
        _ -> ls

findKills :: [(ILKey, Message)] -> [ILKey]
findKills ((k, EnemyDown) : kmsgs) = k : (findKills kmsgs)
findKills ((_, _) : kmsgs) = (findKills kmsgs)
findKills [] = []

findEnemies :: [(ILKey, Message)] -> [ILKey]
findEnemies ((k, PlayerLockedOn) : kmsgs) = k : (findEnemies kmsgs)
findEnemies ((_, _) : kmsgs) = (findEnemies kmsgs)
findEnemies [] = []

toTargetPosition ::
                 ILKey -> Vec3 -> ILKey -> (ILKey, (ILKey, Message))
toTargetPosition iD position contact
  = (contact, (iD, TargetPosition position))

findCoords :: [(ILKey, Message)] -> [(ILKey, Message)]
findCoords ((k, Coord x) : kmsgs)
  = (k, Coord x) : (findCoords kmsgs)
findCoords ((_, _) : kmsgs) = findCoords kmsgs
findCoords [] = []

getCoordFromMsg :: (ILKey, Message) -> Vec3
getCoordFromMsg (_, (Coord xyz)) = xyz
getCoordFromMsg _                = (0,0,0)

aicube ::
       (Double, Double, Double) ->
         (Double, Double, Double) ->
           [(Double, Double, Double)] ->
             String -> (AnimState, AnimState) -> ILKey -> Object
aicube (x, y, z) size waypoints modelname (ua, la) iD
  = (((arr (\ oi -> let gi = oiGameInput oi in (gi, oi)) >>>
         first getT)
        >>>
        arr
          (\ (t, oi) ->
             let hitList = oiHit oi in
               let enemySighted = oiVisibleObjs oi in
                   (hitList, (enemySighted, hitList, oi, t))))
       >>>
       (first
          (arr
             (\ hitList ->
                case (isEvent hitList) of
                    True -> getFire (snd (head (fromEvent hitList)))
                    _ -> Nothing)
             >>> (iPre Nothing <<< identity))
          >>>
          arr
            (\ (hitSource, (enemySighted, hitList, oi, t)) ->
               (hitSource, (enemySighted, hitList, hitSource, oi, t))))
         >>>
         ((first (arr (\ hitSource -> (hitSource /= Nothing)) >>> edge) >>>
             loop
               (arr
                  (\ ((hitev1, (enemySighted, hitList, hitSource, oi, t)),
                      currentHealth)
                     ->
                     ((currentHealth, hitList),
                      (enemySighted, hitSource, hitev1, oi, t)))
                  >>>
                  (first
                     (arr
                        (\ (currentHealth, hitList) ->
                           case (isEvent hitList) of
                               True -> currentHealth - (3)
                               False -> currentHealth)
                        >>> ((iPre 100) <<< identity))
                     >>>
                     arr
                       (\ (currentHealth, (enemySighted, hitSource, hitev1, oi, t)) ->
                          ((currentHealth, enemySighted, hitSource, hitev1, oi, t),
                           currentHealth)))))
            >>>
            arr
              (\ (currentHealth, enemySighted, hitSource, hitev1, oi, t) ->
                 (currentHealth,
                  (currentHealth, enemySighted, hitSource, hitev1, oi, t))))
           >>>
           ((first (arr (\ currentHealth -> (currentHealth <= 0)) >>> edge)
               >>>
               loop
                 (arr
                    (\ ((hitev,
                         (currentHealth, enemySighted, hitSource, hitev1, oi, t)),
                        isDead)
                       ->
                       ((hitev, isDead),
                        (currentHealth, enemySighted, hitSource, hitev, hitev1, oi, t)))
                    >>>
                    (first
                       (arr
                          (\ (hitev, isDead) ->
                             case (isEvent hitev) of
                                 True -> True
                                 _ -> isDead)
                          >>> ((iPre False) <<< identity))
                       >>>
                       arr
                         (\ (isDead,
                             (currentHealth, enemySighted, hitSource, hitev, hitev1, oi, t))
                            ->
                            ((currentHealth, enemySighted, hitSource, hitev, hitev1, isDead,
                              oi, t),
                             isDead)))))
              >>>
              arr
                (\ (currentHealth, enemySighted, hitSource, hitev, hitev1, isDead,
                    oi, t)
                   ->
                   ((enemySighted, isDead),
                    (currentHealth, enemySighted, hitSource, hitev, hitev1, isDead, oi,
                     t))))
             >>>
             (((first
                  (arr
                     (\ (enemySighted, isDead) ->
                        (isEvent enemySighted) && (isDead == False))
                     >>> ((iPre noEvent) <<< edge))
                  >>>
                  loop
                    (arr
                       (\ ((enemyS,
                            (currentHealth, enemySighted, hitSource, hitev, hitev1, isDead, oi,
                             t)),
                           enemy)
                          ->
                          ((enemy, enemySighted),
                           (currentHealth, enemyS, enemySighted, hitSource, hitev, hitev1,
                            isDead, oi, t)))
                       >>>
                       (first
                          (arr
                             (\ (enemy, enemySighted) ->
                                case (isEvent enemySighted) of
                                    True -> enemySighted
                                    False -> enemy)
                             >>> ((iPre noEvent) <<< identity))
                          >>>
                          arr
                            (\ (enemy,
                                (currentHealth, enemyS, enemySighted, hitSource, hitev, hitev1,
                                 isDead, oi, t))
                               ->
                               ((currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                                 hitev1, isDead, oi, t),
                                enemy)))))
                 >>>
                 loop
                   (arr
                      (\ ((currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                           hitev1, isDead, oi, t),
                          targ)
                         ->
                         ((enemySighted, targ),
                          (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                           hitev1, isDead, oi, t)))
                      >>>
                      (first
                         (arr
                            (\ (enemySighted, targ) ->
                               case (isEvent enemySighted) of
                                   True -> (cpos (oldCam (snd (head (fromEvent enemySighted)))))
                                   False -> targ)
                            >>> ((iPre (0, 0, 0)) <<< identity))
                         >>>
                         arr
                           (\ (targ,
                               (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                                hitev1, isDead, oi, t))
                              ->
                              ((currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                                hitev1, isDead, oi, t, targ),
                               targ)))))
                >>>
                arr
                  (\ (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                      hitev1, isDead, oi, t, targ)
                     ->
                     (oi,
                      (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                       hitev1, isDead, oi, t, targ))))
               >>>
               (first
                  (arr (\ oi -> oiMessage oi) >>> ((iPre noEvent) <<< identity))
                  >>>
                  arr
                    (\ (msgs,
                        (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                         hitev1, isDead, oi, t, targ))
                       ->
                       ((isDead, msgs),
                        (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                         hitev1, isDead, msgs, oi, t, targ))))
                 >>>
                 (first
                    (arr
                       (\ (isDead, msgs) ->
                          case (isEvent msgs) && (isDead == False) of
                              True -> case (getTargetPosition (fromEvent msgs)) of
                                          Just _ -> True
                                          _ -> False
                              _ -> False)
                       >>> edge)
                    >>>
                    arr
                      (\ (msgReceived,
                          (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                           hitev1, isDead, msgs, oi, t, targ))
                         ->
                         ((isDead, msgs),
                          (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                           hitev1, isDead, msgReceived, oi, t, targ))))
                   >>>
                   (first
                      (arr
                         (\ (isDead, msgs) ->
                            case (isEvent msgs) && (isDead == False) of
                                True -> case (getTargetPosition2 (fromEvent msgs)) of
                                            Just _ -> True
                                            _ -> False
                                _ -> False)
                         >>> edge)
                      >>>
                      arr
                        (\ (respond2Attack,
                            (currentHealth, enemy, enemyS, enemySighted, hitSource, hitev,
                             hitev1, isDead, msgReceived, oi, t, targ))
                           ->
                           ((enemySighted, msgReceived),
                            (currentHealth, enemy, enemyS, hitSource, hitev, hitev1, isDead,
                             oi, respond2Attack, t, targ))))
                     >>>
                     (first
                        (arr
                           (\ (enemySighted, msgReceived) ->
                              (isNoEvent enemySighted) && (isNoEvent msgReceived))
                           >>> ((iPre noEvent) <<< edge))
                        >>>
                        arr
                          (\ (targetLost1,
                              (currentHealth, enemy, enemyS, hitSource, hitev, hitev1, isDead,
                               oi, respond2Attack, t, targ))
                             ->
                             ((enemyS, targetLost1),
                              (currentHealth, enemy, enemyS, hitSource, hitev, hitev1, isDead,
                               oi, respond2Attack, t, targ, targetLost1))))
                       >>>
                       (first
                          (arr
                             (\ (enemyS, targetLost1) ->
                                ((),
                                 (enemyS `tag` (constant noEvent)) `lMerge`
                                   (targetLost1 `tag` (repeatedly (0.5) (Event ())))))
                             >>> rSwitch (constant noEvent))
                          >>>
                          loop
                            (arr
                               (\ ((_,
                                    (currentHealth, enemy, enemyS, hitSource, hitev, hitev1, isDead,
                                     oi, respond2Attack, t, targ, targetLost1)),
                                   ~(angle, lEndEv, oldPos, uEndEv))
                                  ->
                                  ((angle, enemyS, hitev, lEndEv, oi, oldPos, respond2Attack,
                                    uEndEv),
                                   (currentHealth, enemy, hitSource, hitev, hitev1, isDead, t, targ,
                                    targetLost1)))
                               >>>
                               (first
                                  (arr
                                     (\ (angle, enemyS, hitev, lEndEv, oi, oldPos, respond2Attack,
                                         uEndEv)
                                        ->
                                        ((oi, uEndEv, lEndEv),
                                         (enemyS `tag` (turnToFaceTarget (oldPos, angle))) `lMerge`
                                           respond2Attack
                                           `tag` (turnToFaceTarget (oldPos, angle))
                                           `lMerge` hitev
                                           `tag` playDead oldPos angle))
                                     >>> drSwitch (followWayPoints (x, y, z) waypoints))
                                  >>>
                                  arr
                                    (\ ((newPos, oldPos, angle, ptch, attack,
                                         (upperIdx, lowerIdx)),
                                        (currentHealth, enemy, hitSource, hitev, hitev1, isDead, t,
                                         targ, targetLost1))
                                       ->
                                       ((t, upperIdx),
                                        (angle, attack, currentHealth, enemy, hitSource, hitev,
                                         hitev1, isDead, lowerIdx, newPos, oldPos, ptch, t, targ,
                                         targetLost1))))
                                 >>>
                                 (first (updateAnimSF ua) >>>
                                    arr
                                      (\ ((uEndEv, upperstate),
                                          (angle, attack, currentHealth, enemy, hitSource, hitev,
                                           hitev1, isDead, lowerIdx, newPos, oldPos, ptch, t, targ,
                                           targetLost1))
                                         ->
                                         ((lowerIdx, t),
                                          (angle, attack, currentHealth, enemy, hitSource, hitev,
                                           hitev1, isDead, newPos, oldPos, ptch, targ, targetLost1,
                                           uEndEv, upperstate))))
                                   >>>
                                   (first
                                      (arr (\ (lowerIdx, t) -> (t, lowerIdx)) >>> updateAnimSF la)
                                      >>>
                                      arr
                                        (\ ((lEndEv, lowerstate),
                                            (angle, attack, currentHealth, enemy, hitSource, hitev,
                                             hitev1, isDead, newPos, oldPos, ptch, targ,
                                             targetLost1, uEndEv, upperstate))
                                           ->
                                           ((angle, attack, currentHealth, enemy, hitSource, hitev,
                                             hitev1, isDead, lowerstate, newPos, oldPos, ptch,
                                             targ, targetLost1, upperstate),
                                            (angle, lEndEv, oldPos, uEndEv))))))
                         >>>
                         arr
                           (\ (angle, attack, currentHealth, enemy, hitSource, hitev, hitev1,
                               isDead, lowerstate, newPos, oldPos, ptch, targ, targetLost1,
                               upperstate)
                              ->
                              let f = 1 in
                                ObjOutput{ooObsObjState =
                                            OOSAICube{oosNewCubePos = newPos,
                                                      oosOldCubePos = oldPos, oosCubeSize = size,
                                                      oosCubeAngle = angle, oosCubePitch = ptch,
                                                      upperAnim = upperstate,
                                                      lowerAnim = lowerstate,
                                                      health = currentHealth, target = targ,
                                                      fade = f, modelName = modelname},
                                          ooKillReq = noEvent,
                                          ooSpawnReq =
                                            attack `tag`
                                              [projectile (getMuzzlePoint (oldPos, targ)) iD],
                                          ooSendMessage =
                                            hitev `tag`
                                              (case isDead of
                                                   False -> [(fromJust hitSource, (iD, EnemyDown))]
                                                   _ -> [])
                                              `lMerge` targetLost1
                                              `tag`
                                              [(fst (head (fromEvent enemy)), (iD, PlayerLockedOn))]
                                              `lMerge` hitev1
                                              `tag` [(fromJust hitSource, (iD, PlayerLockedOn2))]}))

getFire :: ObsObjState -> Maybe ILKey
getFire obj
  | isRay obj = Just (firedFrom obj)
  | otherwise = Nothing

getTargetPosition :: [(ILKey, Message)] -> Maybe Vec3
getTargetPosition ((_, TargetPosition pos) : _) = Just pos
getTargetPosition (_ : rest) = getTargetPosition rest
getTargetPosition [] = Nothing

getTargetPosition2 :: [(ILKey, Message)] -> Maybe Vec3
getTargetPosition2 ((_, TargetPosition2 pos) : _) = Just pos
getTargetPosition2 (_ : rest) = getTargetPosition2 rest
getTargetPosition2 [] = Nothing

getMuzzlePoint :: (Vec3, Vec3) -> (Vec3, Vec3)
getMuzzlePoint ((x, y, z), (ox, oy, oz))
  = let (x3, _, z3)
          = normalise $ (vectorSub (ox, oy + 45, oz) (x, y, z))
        (x7, _, z7) = normalise $ (vectorSub (ox, 0, oz) (x, 0, z))
        (x4, _, z4) = normalise $ crossProd (x3, 0, z3) (0, 1, 0)
        (x5, y5, z5)
          = (x + (x7 * (- 18.55)) + (x4 * (9.6)), y + 4,
             z + (z7 * (- 18.55)) + (z4 * (9.6)))
        (x12, y12, z12)
          = normalise $ (vectorSub (ox, oy - 5, oz) (x, y, z))
        (x6, y6, z6)
          = vectorAdd (x5, y5, z5) (x12 * 42, y12 * 42, z12 * 42)
        (x9, y9, z9) = normalise $ (vectorSub (x6, y6, z6) (x5, y5, z5))
        (x10, y10, z10) = normalise $ crossProd (x9, y9, z9) (0, 1, 0)
        (x13, y13, z13)
          = normalise $ crossProd (x9, y9, z9) (x10, y10, z10)
        muzzlePoint
          = vectorAdd (x6, y6, z6)
              (x13 * (- 9.5), y13 * (- 9.5), z13 * (- 9.5))
        muzzleEnd
          = vectorAdd (x5, y5, z5)
              (x13 * (- 9.5), y13 * (- 9.5), z13 * (- 9.5))
        fireVec = normalise $ (vectorSub muzzlePoint muzzleEnd)
      in (muzzlePoint, fireVec)

falling :: SF (Bool, GameInput, Double) Double
falling
  = (loop
       (arr
          (\ ((lnd, _, dt), pos) ->
             case lnd of
                 True -> (- 0.5)
                 False -> (pos - (6 * 200 * dt)))
          >>> (((iPre 0) <<< identity) >>> arr (\ pos -> (pos, pos)))))

turnToFaceTarget ::
                 (Vec3, Double) ->
                   SF (ObjInput, Event (), Event ())
                     (Vec3, Vec3, Double, Double, Event (), (Int, Int))
turnToFaceTarget (currentPos, initialAngle)
  = (arr
       (\ (oi, ev1, ev2) ->
          let gi = oiGameInput oi in
            let clippedPos = oiCollisionPos oi in
              let grounded = oiOnLand oi in
                (gi, (clippedPos, ev1, ev2, gi, grounded, oi)))
       >>>
       ((first getDt >>>
           loop
             (arr
                (\ ((dt, (clippedPos, ev1, ev2, gi, grounded, oi)), count) ->
                   (count, (clippedPos, dt, ev1, ev2, gi, grounded, oi)))
                >>>
                (first (arr (\ count -> count + 1) >>> ((iPre 0) <<< identity)) >>>
                   arr
                     (\ (count, (clippedPos, dt, ev1, ev2, gi, grounded, oi)) ->
                        ((clippedPos, count, dt, ev1, ev2, gi, grounded, oi), count)))))
          >>>
          arr
            (\ (clippedPos, count, dt, ev1, ev2, gi, grounded, oi) ->
               (clippedPos, (count, dt, ev1, ev2, gi, grounded, oi))))
         >>>
         (first ((iPre currentPos) <<< identity) >>>
            arr
              (\ ((ox1, oy1, oz1), (count, dt, ev1, ev2, gi, grounded, oi)) ->
                 ((count, ox1, oy1, oz1), (dt, ev1, ev2, gi, grounded, oi))))
           >>>
           (first
              (arr
                 (\ (count, ox1, oy1, oz1) ->
                    case (count > (3 :: Int) && (ox1, oy1, oz1) /= currentPos) of
                        True -> (ox1, oy1, oz1)
                        _ -> currentPos)
                 >>> identity)
              >>>
              arr
                (\ ((ox, oy, oz), (dt, ev1, ev2, gi, grounded, oi)) ->
                   ((dt, gi, grounded), (ev1, ev2, oi, ox, oy, oz))))
             >>>
             ((((((first
                     (arr (\ (dt, gi, grounded) -> (grounded, gi, dt)) >>>
                        ((iPre 0) <<< falling))
                     >>>
                     arr
                       (\ (yVel, (ev1, ev2, oi, ox, oy, oz)) ->
                          let enemySighted = oiVisibleObjs oi in
                            (enemySighted, ev1, ev2, ox, oy, oz, yVel)))
                    >>>
                    loop
                      (arr
                         (\ ((enemySighted, ev1, ev2, ox, oy, oz, yVel), targetAnglei) ->
                            ((enemySighted, ox, oy, oz, targetAnglei),
                             (enemySighted, ev1, ev2, ox, oy, oz, yVel)))
                         >>>
                         (first
                            (arr
                               (\ (enemySighted, ox, oy, oz, targetAnglei) ->
                                  case (isEvent enemySighted) of
                                      True -> getAngle
                                                ((ox, oy, oz),
                                                 (cpos
                                                    (oldCam (snd (head (fromEvent enemySighted))))))
                                      _ -> targetAnglei)
                               >>> ((iPre initialAngle) <<< identity))
                            >>>
                            arr
                              (\ (targetAnglei, (enemySighted, ev1, ev2, ox, oy, oz, yVel)) ->
                                 ((enemySighted, ev1, ev2, ox, oy, oz, targetAnglei, yVel),
                                  targetAnglei)))))
                   >>>
                   loop
                     (arr
                        (\ ((enemySighted, ev1, ev2, ox, oy, oz, targetAnglei, yVel),
                            angle)
                           ->
                           let targetAngle
                                 = case
                                     (abs (angle - targetAnglei) <
                                        abs (angle - (targetAnglei + 360)))
                                     of
                                       True -> targetAnglei
                                       False -> targetAnglei + 360
                             in
                             let angularV
                                   = case (True) of
                                         True -> case (abs (angle - targetAngle) > 2) of
                                                     True -> case (angle < targetAngle) of
                                                                 True -> 270
                                                                 _ -> - 270
                                                     False -> (targetAngle - angle)
                                         False -> 0
                               in
                               (angularV,
                                (enemySighted, ev1, ev2, ox, oy, oz, targetAngle, yVel)))
                        >>>
                        (first ((initialAngle +) ^<< integral) >>>
                           arr
                             (\ (angle, (enemySighted, ev1, ev2, ox, oy, oz, targetAngle, yVel))
                                ->
                                let legState
                                      = case (abs (angle - targetAngle) < 2) of
                                            True -> idleLegs
                                            _ -> turn
                                  in
                                  ((ev2, legState),
                                   (angle, enemySighted, ev1, legState, ox, oy, oz, targetAngle,
                                    yVel))))
                          >>>
                          (first
                             (arr
                                (\ (ev2, legState) -> ((legState == idleLegs) && (isEvent ev2)))
                                >>> edge)
                             >>>
                             arr
                               (\ (switch2idle,
                                   (angle, enemySighted, ev1, legState, ox, oy, oz, targetAngle,
                                    yVel))
                                  ->
                                  (legState,
                                   (angle, enemySighted, ev1, ox, oy, oz, switch2idle, targetAngle,
                                    yVel))))
                            >>>
                            (first (arr (\ legState -> (legState == turn)) >>> edge) >>>
                               arr
                                 (\ (turning,
                                     (angle, enemySighted, ev1, ox, oy, oz, switch2idle,
                                      targetAngle, yVel))
                                    ->
                                    ((switch2idle, turning),
                                     (angle, enemySighted, ev1, ox, oy, oz, targetAngle, yVel))))
                              >>>
                              (first
                                 (arr
                                    (\ (switch2idle, turning) ->
                                       ((),
                                        turning `tag` (constant turn) `lMerge` switch2idle `tag`
                                          (constant idleLegs)))
                                    >>> drSwitch (constant stand))
                                 >>>
                                 arr
                                   (\ (legsAnim,
                                       (angle, enemySighted, ev1, ox, oy, oz, targetAngle, yVel))
                                      ->
                                      ((angle, enemySighted, ev1, legsAnim, ox, oy, oz, targetAngle,
                                        yVel),
                                       angle)))))
                  >>>
                  loop
                    (arr
                       (\ ((angle, enemySighted, ev1, legsAnim, ox, oy, oz, targetAngle,
                            yVel),
                           targetPitch)
                          ->
                          ((enemySighted, ox, oy, oz, targetPitch),
                           (angle, enemySighted, ev1, legsAnim, ox, oy, oz, targetAngle,
                            yVel)))
                       >>>
                       (first
                          (arr
                             (\ (enemySighted, ox, oy, oz, targetPitch) ->
                                case (isEvent enemySighted) of
                                    True -> getVertAngle
                                              ((ox, oy, oz),
                                               vectorAdd
                                                 (cpos
                                                    (oldCam (snd (head (fromEvent enemySighted)))))
                                                 (0, - 5, 0))
                                    _ -> targetPitch)
                             >>> ((iPre 0) <<< identity))
                          >>>
                          arr
                            (\ (targetPitch,
                                (angle, enemySighted, ev1, legsAnim, ox, oy, oz, targetAngle,
                                 yVel))
                               ->
                               ((angle, enemySighted, ev1, legsAnim, ox, oy, oz, targetAngle,
                                 targetPitch, yVel),
                                targetPitch)))))
                 >>>
                 loop
                   (arr
                      (\ ((angle, enemySighted, ev1, legsAnim, ox, oy, oz, targetAngle,
                           targetPitch, yVel),
                          ptch)
                         ->
                         let angularVP
                               = case (abs (ptch - targetPitch) > 2) of
                                     True -> case (targetPitch < ptch) of
                                                 True -> - 90
                                                 _ -> 90
                                     False -> (targetPitch - ptch)
                           in
                           (angularVP,
                            (angle, enemySighted, ev1, legsAnim, ox, oy, oz, targetAngle,
                             targetPitch, yVel)))
                      >>>
                      (first ((0 +) ^<< integral) >>>
                         arr
                           (\ (ptch,
                               (angle, enemySighted, ev1, legsAnim, ox, oy, oz, targetAngle,
                                targetPitch, yVel))
                              ->
                              ((angle, enemySighted, ev1, legsAnim, ox, oy, oz, ptch,
                                targetAngle, targetPitch, yVel),
                               ptch)))))
                >>>
                arr
                  (\ (angle, enemySighted, ev1, legsAnim, ox, oy, oz, ptch,
                      targetAngle, targetPitch, yVel)
                     ->
                     (ev1,
                      (angle, enemySighted, legsAnim, ox, oy, oz, ptch, targetAngle,
                       targetPitch, yVel))))
               >>>
               (first (arr (\ ev1 -> (isEvent ev1)) >>> ((iPre noEvent) <<< edge))
                  >>>
                  arr
                    (\ (attack,
                        (angle, enemySighted, legsAnim, ox, oy, oz, ptch, targetAngle,
                         targetPitch, yVel))
                       ->
                       ((angle, enemySighted, ptch, targetAngle, targetPitch),
                        (angle, attack, legsAnim, ox, oy, oz, ptch, yVel))))
                 >>>
                 (first
                    (arr
                       (\ (angle, enemySighted, ptch, targetAngle, targetPitch) ->
                          case
                            ((abs (ptch - targetPitch) < 6) && (abs (angle - targetAngle) < 6)
                               && isEvent enemySighted)
                            of
                              True -> attack1
                              False -> stand)
                       >>> ((iPre stand) <<< identity))
                    >>>
                    arr
                      (\ (torsoAnim, (angle, attack, legsAnim, ox, oy, oz, ptch, yVel))
                         ->
                         ((ox, oy + yVel, oz), (ox, oy, oz), angle, ptch, attack,
                          (torsoAnim, legsAnim)))))

followWayPoints ::
                Vec3 ->
                  [Vec3] ->
                    SF (ObjInput, Event (), Event ())
                      (Vec3, Vec3, Double, Double, Event (), (Int, Int))
followWayPoints (x, y, z) waypoints
  = (arr
       (\ (oi, ev1, ev2) ->
          let gi = oiGameInput oi in
            let clippedPos = oiCollisionPos oi in
              let grounded = oiOnLand oi in
                (clippedPos, (ev1, ev2, gi, grounded, oi)))
       >>>
       (first ((iPre (x, y, z)) <<< identity) >>>
          arr
            (\ ((ox, oy, oz), (ev1, ev2, gi, grounded, oi)) ->
               (gi, (ev1, ev2, gi, grounded, oi, ox, oy, oz))))
         >>>
         (first getT >>>
            arr
              (\ (_, (ev1, ev2, gi, grounded, oi, ox, oy, oz)) ->
                 (gi, (ev1, ev2, gi, grounded, oi, ox, oy, oz))))
           >>>
           (first getDt >>>
              arr
                (\ (dt, (ev1, ev2, gi, grounded, oi, ox, oy, oz)) ->
                   ((dt, gi, grounded), (dt, ev1, ev2, oi, ox, oy, oz))))
             >>>
             (first
                (arr (\ (dt, gi, grounded) -> (grounded, gi, dt)) >>>
                   ((iPre 0) <<< falling))
                >>>
                loop
                  (arr
                     (\ ((yVel, (dt, ev1, ev2, oi, ox, oy, oz)), wpl) ->
                        (wpl, (dt, ev1, ev2, oi, ox, oy, oz, yVel)))
                     >>>
                     (first ((iPre (cycle waypoints)) <<< identity) >>>
                        arr
                          (\ (wps, (dt, ev1, ev2, oi, ox, oy, oz, yVel)) ->
                             let [wp1, wp2] = take 2 wps in
                               let (pastWp, (dx, _, dz)) = stepdist wp1 wp2 (ox, oy, oz) 100 dt
                                 in
                                 (pastWp,
                                  (dx, dz, ev1, ev2, oi, ox, oy, oz, pastWp, wp2, wps, yVel))))
                       >>>
                       (first edge >>>
                          arr
                            (\ (pastEv,
                                (dx, dz, ev1, ev2, oi, ox, oy, oz, pastWp, wp2, wps, yVel))
                               ->
                               let angle = getAngle ((ox, oy, oz), (ox + dx, oy + yVel, oz + dz))
                                 in
                                 let newPos = (ox + dx, oy + yVel, oz + dz) in
                                   ((angle, ev1, ev2, newPos, oi, pastEv, wp2),
                                    (angle, newPos, ox, oy, oz, pastWp, wps))))
                         >>>
                         (first
                            (arr
                               (\ (angle, ev1, ev2, newPos, oi, pastEv, wp2) ->
                                  ((oi, ev1, ev2),
                                   pastEv `tag` (turnToNextWp angle (getAngle (newPos, wp2)))))
                               >>>
                               rSwitch
                                 (constant (True, False, (getAngle ((x, y, z), head waypoints)))))
                            >>>
                            arr
                              (\ ((notturning, largeEnough, turnAngle),
                                  (angle, newPos, ox, oy, oz, pastWp, wps))
                                 ->
                                 let wpl
                                       = case (pastWp) of
                                             True -> case (not largeEnough) of
                                                         True -> (tail wps)
                                                         _ -> case notturning of
                                                                  True -> (tail wps)
                                                                  _ -> wps
                                             _ -> wps
                                   in
                                   ((angle, largeEnough, newPos, notturning, ox, oy, oz, turnAngle),
                                    wpl)))))
               >>>
               arr
                 (\ (angle, largeEnough, newPos, notturning, ox, oy, oz, turnAngle)
                    ->
                    let holdAngle
                          = case (not largeEnough) of
                                True -> angle
                                _ -> case notturning of
                                         False -> turnAngle
                                         _ -> angle
                      in
                      let legAnim
                            = case (not largeEnough) of
                                  True -> walk
                                  _ -> case (notturning) of
                                           True -> walk
                                           _ -> turn
                        in
                        (newPos, (ox, oy, oz), holdAngle, 0, noEvent, (stand, legAnim))))

turnToNextWp ::
             Double ->
               Double -> SF (ObjInput, Event (), Event ()) (Bool, Bool, Double)
turnToNextWp currentangle nextAngle
  = (((arr
         (\ (_, _, lev) ->
            let targetAngle
                  = case
                      (abs (currentangle - nextAngle) <
                         abs (currentangle - (nextAngle + 360)))
                      of
                        True -> nextAngle
                        False -> nextAngle + 360
              in (lev, targetAngle))
         >>>
         loop
           (arr
              (\ ((lev, targetAngle), angle) ->
                 let angularV
                       = case (abs (angle - targetAngle) > 3) of
                             True -> case (angle < targetAngle) of
                                         True -> 360
                                         _ -> - 360
                             False -> (targetAngle - angle)
                   in (angularV, (lev, targetAngle)))
              >>>
              (first ((currentangle +) ^<< integral) >>>
                 arr
                   (\ (angle, (lev, targetAngle)) ->
                      ((angle, lev, targetAngle), angle)))))
        >>>
        arr
          (\ (angle, lev, targetAngle) ->
             let legState
                   = case (abs (angle - targetAngle) < 3) of
                         True -> idleLegs
                         _ -> turn
               in ((legState, lev), (angle, targetAngle))))
       >>>
       (first
          (arr (\ (legState, lev) -> (legState == idleLegs && isEvent lev))
             >>> ((iPre noEvent) <<< edge))
          >>>
          first
            (arr (\ switch2idle -> ((), switch2idle `tag` (constant True))) >>>
               rSwitch (constant False)))
         >>>
         arr
           (\ (ret, (angle, targetAngle)) ->
              (ret, (abs (currentangle - targetAngle) > 30), angle)))

stepdist ::
         Vec3 -> Vec3 -> Vec3 -> Double -> Double -> (Bool, Vec3)
stepdist (wx1, _, wz1) (_, _, _) (x, _, z) vel dt
  = let (dx, _, dz) = normalise $ vectorSub (wx1, 0, wz1) (x, 0, z)
        distance = sqrt (((x - wx1) * (x - wx1)) + ((z - wz1) * (z - wz1)))
        remvel = distance * (distance / (vel * dt))
      in
      case (distance > (vel * dt)) of
          True -> (False, (dx * vel * dt, 0, dz * vel * dt))
          False -> (True, (dx * remvel, 0, dz * remvel))

playDead ::
         Vec3 ->
           Double ->
             SF (ObjInput, Event (), Event ())
               (Vec3, Vec3, Double, Double, Event (), (Int, Int))
playDead start angle
  = (arr (\ (_, ev1, _) -> ev1) >>>
       (notYet >>> arr (\ ev -> ((), ev `tag` (constant dead1)))) >>>
         (drSwitch (constant death1) >>>
            arr
              (\ death -> (start, start, angle, 0, noEvent, (death, death)))))

getAngle :: (Vec3, Vec3) -> Double
getAngle ((x, _, z), (vx, _, vz))
  = let angle
          = acos $
              dotProd (normalise $ vectorSub (vx, 0, vz) (x, 0, z)) (1, 0, 0)
      in
      case (vz > z) of
          False -> (angle * 180 / pi)
          True -> (360 - (angle * 180 / pi))

getVertAngle :: (Vec3, Vec3) -> Double
getVertAngle ((x, y, z), (vx, vy, vz))
  = let angle1
          = acos $
              dotProd (normalise $ vectorSub (vx, vy, vz) (x, y, z)) (0, 1, 0)
      in ((angle1 * 180 / pi) - 90)

updateAnimSF :: AnimState -> SF (Double, Int) (Event (), AnimState)
updateAnimSF iAnim
  = (loop
       (arr
          (\ ((tme, animIndex), anim2) ->
             ((anim2, animIndex, tme), animIndex))
          >>>
          (first
             (arr (\ (anim2, animIndex, tme) -> (animIndex, tme, anim2)) >>>
                ((iPre (False, iAnim)) <<< arr updateAnim))
             >>>
             arr
               (\ ((hasLooped, anim2), animIndex) ->
                  ((anim2, animIndex, hasLooped), anim2))))
       >>>
       arr
         (\ (anim2, animIndex, hasLooped) ->
            (case hasLooped of
                 True -> case (animIndex == dead1) of
                             True -> (noEvent, anim2)
                             False -> (Event (), anim2)
                 False -> (noEvent, anim2))))

moves :: SF (Double, Camera) Camera
moves
  = (arr
       (\ (speed, cam) ->
          let (x, y, z) = (cpos cam)
              (vpx, vpy, vpz) = (viewPos cam)
              strafevec
                = normalise
                    (crossProd (vectorSub (viewPos cam) (cpos cam)) (upVec cam))
              (vx, _, vz) = normalise (crossProd (upVec cam) strafevec)
              newx = (vx * speed)
              newz = (vz * speed)
              newvx = (vx * speed)
              newvz = (vz * speed)
            in
            Camera{cpos = (x + newx, y, z + newz),
                   viewPos = (vpx + newvx, vpy, vpz + newvz), upVec = (upVec cam)}))

strafes :: SF (Double, Camera) Camera
strafes
  = (arr
       (\ (speed, cam) ->
          let (sx, _, sz)
                = normalise
                    (crossProd (vectorSub (viewPos cam) (cpos cam)) (upVec cam))
              (x, y, z) = (cpos cam)
              (vx, vy, vz) = (viewPos cam)
              newx = (sx * speed)
              newz = (sz * speed)
              newvx = (sx * speed)
              newvz = (sz * speed)
            in
            Camera{cpos = (x + newx, y, z + newz),
                   viewPos = (vx + newvx, vy, vz + newvz), upVec = (upVec cam)}))

movementKS :: Double -> SF GameInput Double
movementKS speed
  = (keyStat >>>
       loop
         (arr (\ (key, v) -> nextSpeed key v) >>>
            (((iPre 0) <<< identity) >>> arr (\ v -> (v, v)))))
  where nextSpeed key v
          | key == Event ('w', True) = speed
          | key == Event ('s', True) = - speed
          | (key == Event ('w', False) || key == Event ('s', False)) = 0
          | otherwise = v

strafeKS :: Double -> SF GameInput Double
strafeKS speed
  = (keyStat >>>
       loop
         (arr (\ (key, v) -> nextSpeed key v) >>>
            (((iPre 0) <<< identity) >>> arr (\ v -> (v, v)))))
  where nextSpeed key v
          | key == Event ('d', True) = speed
          | key == Event ('a', True) = - speed
          | (key == Event ('d', False) || key == Event ('a', False)) = 0
          | otherwise = v

fallingp :: SF (Bool, GameInput) Double
fallingp
  = (arr (\ (lnd, gi) -> (gi, lnd)) >>>
       (first keyStat >>> arr (\ (key, lnd) -> (key, (key, lnd)))) >>>
         (first (arr (\ key -> key == Event ('e', True)) >>> arr jump2Vel)
            >>> arr (\ (_, (key, lnd)) -> ((key, lnd), (key, lnd))))
           >>>
           (first
              (arr (\ (key, lnd) -> key == Event ('e', True) && (lnd == True))
                 >>> arr bool2Ev)
              >>> arr (\ (jumping, (key, lnd)) -> (lnd, (jumping, key, lnd))))
             >>>
             ((first (arr (\ lnd -> (lnd == True)) >>> edge) >>>
                 loop
                   (arr
                      (\ ((landed, (jumping, key, lnd)), middleOfJump) ->
                         ((key, lnd, middleOfJump), (jumping, lnd, landed)))
                      >>>
                      (first
                         (arr
                            (\ (key, lnd, middleOfJump) ->
                               case
                                 (middleOfJump == False && key == Event ('e', True) && lnd == True)
                                 of
                                   True -> True
                                   False -> case (lnd == True) of
                                                True -> False
                                                False -> middleOfJump)
                            >>> (iPre False <<< identity))
                         >>>
                         arr
                           (\ (middleOfJump, (jumping, lnd, landed)) ->
                              ((jumping, lnd, landed, middleOfJump), middleOfJump)))))
                >>>
                arr
                  (\ (jumping, lnd, landed, middleOfJump) ->
                     ((lnd, middleOfJump), (jumping, landed))))
               >>>
               (first
                  (arr
                     (\ (lnd, middleOfJump) ->
                        (lnd == False && middleOfJump == False))
                     >>> edge)
                  >>>
                  arr
                    (\ (notlanded, (jumping, landed)) ->
                       ((),
                        (jumping `tag` falling' (- 200 :: Double) (40 :: Double)) `lMerge`
                          (landed `tag` constant (- 5.0e-2))
                          `lMerge`
                          (notlanded `tag` falling' (- 200 :: Double) (0 :: Double)))))
                 >>> drSwitch (falling' (- 200 :: Double) (0 :: Double)))

falling' :: Double -> Double -> SF () Double
falling' grav int
  = (arr (\ () -> grav) >>> (integral >>> arr (\ vel -> (vel + int))) >>> integral)

bool2Ev :: Bool -> Event ()
bool2Ev b
  | b = Event ()
  | otherwise = noEvent

jump2Vel :: Bool -> Double
jump2Vel b
  | b == True = 40
  | otherwise = 0
