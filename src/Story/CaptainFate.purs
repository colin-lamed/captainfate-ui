module Story.CaptainFate
  ( story
  ) where

import Motor.Story


white  = RGB 255 255 255
orange = RGB 255 165 0
cyan   = RGB 0   255 255

story ∷ StoryDef
story = do

  setSTitle "Captain Fate"

  let streetBooth       = Oid "booth"
      streetCafe        = Oid "cafe"
      streetPedestrians = Oid "pedestrians"
      streetSidewalk    = Oid "sidewalk"
      boothCafe         = Oid "cafe from booth"
      boothSidewalk     = Oid "sidewalk from booth"
      cafeCounter       = Oid "counter"
      cafeFood          = Oid "food"
      cafeMenu          = Oid "menu"
      cafeCustomers     = Oid "customers"
      cafeBenny         = Oid "benny"
      cafeToiletDoor    = Oid "toilet door"
      cafeOutsideDoor   = Oid "outside door"
      coffee            = Oid "coffee"
      key               = Oid "key"
      note              = Oid "note"
      toiletLightSwitch = Oid "light switch"
      toiletDoor        = Oid "toilet door!"
      toiletLavatory    = Oid "lavatory"
      coin              = Oid "coin"
      costume           = Oid "costume"
      clothes           = Oid "clothes"

      streetR           = Rid "street"
      boothR            = Rid "booth"
      cafeR             = Rid "cafe"
      toiletDarknessR   = Rid "toilet darkness"
      toiletR           = Rid "toilet"

      playerA           = Aid "player"
      bennyA            = Aid "benny"
      customerA         = Aid "customer"

  streetImg           ← mkImg "img/CaptainFate/street.png"
  boothImg            ← mkImg "img/CaptainFate/booth.png"
  coffeeImg           ← mkImg "img/CaptainFate/coffee.png"
  coffeeIImg          ← mkImg "img/CaptainFate/coffee-inventory.png"
  keyImg              ← mkImg "img/CaptainFate/key.png"
  keyIImg             ← mkImg "img/CaptainFate/key-inventory.png"
  cafeImg             ← mkImg "img/CaptainFate/cafe.png"
  toiletDoorOpenImg   ← mkImg "img/CaptainFate/toiletDoorOpen.png"
  toiletDoorClosedImg ← mkImg "img/CaptainFate/toiletDoorClosed.png"
  coinImg             ← mkImg "img/CaptainFate/coin.png"
  coinIImg            ← mkImg "img/CaptainFate/coin-inventory.png"
  toiletImg           ← mkImg "img/CaptainFate/toilet.png"
  toiletDarknessImg   ← mkImg "img/CaptainFate/toilet-darkness.png"
  costumeIImg         ← mkImg "img/CaptainFate/costume-inventory.png"
  clothesIImg         ← mkImg "img/CaptainFate/clothes-inventory.png"


  -- verbs
  -- general
  walkToV  ← mkVerb "Walk to"
  lookAtV  ← mkVerb "Look at"
  pickUpV  ← mkVerb "Pick up"
  useV     ← mkVerb "Use"
  openV    ← mkVerb "Open"
  closeV   ← mkVerb "Close"
  -- specific to this game
  talkToV  ← mkVerb "Talk to"
  giveV    ← mkVerb "Give"
  -- following can all be replaced with Use when adjusted to have optional prep
  turnOnV  ← mkVerb "Turn on" -- could be push
  turnOffV ← mkVerb "Turn off" -- could be pull

-- TODO following options? note, no walkto - is implicit?
-- open, pick up, push
-- close, look at, pull
-- give, talk to, use


  let allVerbs = [lookAtV, pickUpV, useV, openV, closeV, giveV, talkToV, turnOnV, turnOffV] -- except walkToV


  -- states
  coffeeGivenS    ← mkState "coffeeGivenS"    false
  needPayCoffeeS  ← mkState "needPayCoffeeS"  false
  noteReadS       ← mkState "noteReadS"       false
  toiletDoorOpenS ← mkState "toiletDoorOpenS" false
  toiletLightOnS  ← mkState "toiletLightOnS"  false
  costumeWornS    ← mkState "costumeWornS"    false

  mkActor playerA $ do
    setAName "you"
    setAColor white

  mkActor bennyA $ do
    setAName "benny"
    setAColor orange

  mkActor customerA $ do
    setAName "customer"
    setAColor cyan

  setSDefaultVerb walkToV
  setSInitialRoom streetR
  setSInitialActor playerA

  let playerSay           = say playerA
      playerSayAndThen    = sayAndThen playerA
      bennySay            = say bennyA
      bennySayAndThen     = sayAndThen bennyA
      customersSay        = say customerA
      customersSayAndThen = sayAndThen customerA

  let objectTemplate = do
        setOSuggestedVerb $ pure lookAtV
        addOInterceptor [lookAtV] $ playerSay "I don't see anything special\nabout it"

  let applianceTemplate = do
        objectTemplate
        addOInterceptor [pickUpV] $
          playerSay "Even though your SCULPTED adamantine muscles are up to the task, you don't favour property damage."
        -- TODO what about default behaviour refering to self? self being added to state before invocation?
        -- #{:take :pull :push :push-dir} (fn [] (str (The self) " is too heavy for that."))

--------------------------------------------------------------------------------
-- ON STREET -------------------------------------------------------------------
--------------------------------------------------------------------------------

  mkObject streetBooth $ do
    applianceTemplate
    setOName "booth"
    -- useDistance, usePosition, useDirection
    -- state? (open/closed)

    addOInterceptor [lookAtV] $ playerSay "It's one of the old picturesque models, a red cabin with room for one caller."
    addOInterceptor [openV]   $ playerSay "The booth is already open."
    addOInterceptor [closeV]  $ playerSay "There's no way to close this booth."
    addOInterceptor [walkToV] $ do
      playerSayAndThen "With implausible celerity, you dive inside the phone booth." $
        moveTo boothR

    -- alternatively (since vocab is not open ended):
     -- lookAtR = say "It's a door",
     -- useR = (\noun → say "I cant use door with " ++ show now)



  mkObject streetCafe $ do
    applianceTemplate
    setOName "Benny's cafe"
    addOInterceptor [lookAtV] $ playerSay "The town's favourite for a quick snack, Benny's cafe has a 50's ROCKETSHIP look."
    addOInterceptor [walkToV] $ moveTo cafeR

  mkObject streetPedestrians $ do
    objectTemplate
    setOName "pedestrians"
    addOInterceptor allVerbs  $ playerSay "The passing pedestrians are of NO concern to you."
    addOInterceptor [lookAtV] $ playerSay "They're just PEOPLE going about their daily HONEST business."

  mkObject streetSidewalk $ do
    applianceTemplate
    setOName "sidewalk"
    addOInterceptor [lookAtV] $ playerSay "it looks JUST like any other sidewalk in the CITY!"

  mkRoom streetR $ do
    setRImage $ pure streetImg
    setRHotspots $
      [ Tuple streetBooth       [Tuple  45.0 310.0, Tuple  48.0  87.0, Tuple 186.0  77.0, Tuple 196.0 266.0, Tuple 132.0 329.0, Tuple 45.0 310.0]
      , Tuple streetCafe        [Tuple 608.0 150.0, Tuple 756.0 180.0, Tuple 748.0 372.0, Tuple 647.0 335.0, Tuple 608.0 150.0]
      , Tuple streetPedestrians [Tuple 950.0 352.0, Tuple 995.0 360.0, Tuple 986.0 501.0, Tuple 952.0 491.0, Tuple 950.0 352.0]
      , Tuple streetSidewalk    [Tuple 340.0 283.0, Tuple 760.0 411.0, Tuple 754.0 502.0, Tuple 281.0 381.0, Tuple 340.0 283.0]
      ]


--------------------------------------------------------------------------------
-- INSIDE BOOTH ----------------------------------------------------------------
--------------------------------------------------------------------------------

  mkObject boothCafe $ do
    objectTemplate
    setOName "cafe"
    addOInterceptor [lookAtV] $ playerSay "From this VANTAGE point, you are rewarded with a broad view of the sidewalk and the entrance to Benny's cafe."

  mkObject boothSidewalk $ do
    applianceTemplate
    setOName "sidewalk"
    addOInterceptor [lookAtV] $ playerSay "From this VANTAGE point, you are rewarded with a broad view of the sidewalk and the entrance to Benny's cafe."
    addOInterceptor [walkToV] $ moveTo streetR

  mkRoom boothR $ do
    setRImage $ pure boothImg
    setRHotspots $
      [ Tuple boothCafe     [Tuple  20.0 176.0, Tuple 315.0 75.0, Tuple  57.0 265.0, Tuple 237.0 515.0, Tuple  18.0 517.0, Tuple 20.0 176.0]
      , Tuple boothSidewalk [Tuple 305.0 505.0, Tuple 499.0 87.0, Tuple 617.0  90.0, Tuple 845.0 525.0, Tuple 305.0 505.0]
      ]

--------------------------------------------------------------------------------
-- INSIDE CAFE -----------------------------------------------------------------
--------------------------------------------------------------------------------

  mkObject cafeCounter $ do
    applianceTemplate
    setOName "counter"
    addOInterceptor [lookAtV] $ playerSay "The counter is made of an astonishing ALLOY of metals, STAIN-PROOF, SPILL-RESISTANT and VERY EASY to clean. Customers enjoy their snacks with UTTER tranquillity, safe in the notion that the counter can take it all."

  mkObject cafeFood $ do
    objectTemplate
    setOName "food"
    addOInterceptor allVerbs $ playerSay "There is no time for FOOD right now."

  mkObject cafeMenu $ do
    objectTemplate
    setOName "menu"
    addOInterceptor [lookAtV] $ playerSay "The menu board lists Benny's food and drinks, along with their prices. Too bad you've never learnt how to read, but luckily there is a picture of a big cup of coffee among the incomprehensible writing."
    addOInterceptor [pickUpV] $ playerSay "The board is mounted on the wall behind Benny. Besides, it's useless WRITING."

  let theComments =
        [ customersSayAndThen "Didn't know there was a circus in town," $
            customersSay "Seems like the clowns have the day off."
        , customersSay "These fashion designers don't know what to do to show off."
        , customersSayAndThen "Must be carnival again," $
            customersSay "Time sure flies."
        , customersSayAndThen "Bad thing about big towns,"  $
            customersSay "is you get the damnedest bugs coming out from toilets."
        , customersSayAndThen "I sure WISH I could go to work in my pyjamas," $
            customersSay "It looks SO comfortable."
        ]

  let sayRandom = do
        res ← takeAtRandom theComments
        res
        after 10000 sayRandom -- TODO until said them all?

  mkObject cafeCustomers $ do
    objectTemplate
    setOName "customers"
    addOInterceptor [lookAtV] $ do
      costumeWorn ← getState costumeWornS
      if costumeWorn
        then
          playerSayAndThen "Most seem to be concentrating on their food, but some are" $
            playerSayAndThen "looking at me quite blatantly." $
              playerSay" Must be the MIND-BEFUDDLING colours of my costume."
        else
          playerSayAndThen "A group of HELPLESS and UNSUSPECTING mortals, the kind" $
            playerSayAndThen "Captain FATE swore to DEFEND the day his parents choked" $
              playerSay "on a DEVIOUS slice of RASPBERRY PIE."
    addOInterceptor [talkToV] $ do
      costumeWorn ← getState costumeWornS
      if costumeWorn
        then playerSay "People seem to MISTRUST the look of my FABULOUS costume."
        else playerSay "As John Covarth, I attract LESS interest than Benny's food."
    setOOnEnter $ do
      -- TODO should continue until all comments are said (not picking comment at random)
      --      and 50% chance of a little sleep before displaying the next comment
      --      requires running as daemon (being stopped in onExit?)
      costumeWorn ← getState costumeWornS
      -- speak ← withRandom $ random
      when costumeWorn $ -- && speak
        after 2000 sayRandom
    setOOnExit $ playerSay "exit - customers"

  let bennyTalkOptions ∷ SW Unit
      bennyTalkOptions = talkOptions $
        [ -- option only available if : getState noteReadS ?
          talkOption "Can I have the toilet key?" do
            hasKey ← hasInInventory key
            coffeeGiven ← getState coffeeGivenS
            if hasKey then bennySayAndThen "Last place I saw that key, it was in YOUR possession" $
                                      bennySayAndThen "Be sure to pure it before you leave." $
                                        bennyTalkOptions
              else if not coffeeGiven then bennySayAndThen "Toilet is only fer customers"
                                             bennyTalkOptions
              else bennySayAndThen "Here's the key" do
                                      addToCurrentRoom key (Tuple 969.0 250.0)
                                      bennyTalkOptions
        , -- option only available if looked at menu?
          talkOption "Can I have a coffee" do
            coffeeGiven ← getState coffeeGivenS
            if coffeeGiven then bennySayAndThen "I already gave you a coffee"
                                    bennyTalkOptions
              else bennySayAndThen "That will be one quidbuck, sir." $ do
                                    addToCurrentRoom coffee (Tuple 858.0 230.0)
                                    setState coffeeGivenS true
                                    setState needPayCoffeeS true
                                    bennyTalkOptions
        , talkOption "I must go now" restoreVerbs
        ]


  mkObject cafeBenny $ do
    objectTemplate
    setOName "benny"
    addOInterceptor [lookAtV] $ playerSay "A deceptively FAT man of uncanny agility."
    addOInterceptor [talkToV] $ bennyTalkOptions
      -- TODO handle absence of interceptor on target with interceptor on noun? e.g.
      --addOInterceptor2 useV (\noun → playerSay "I don't think he'll want that.")
    setOSuggestedVerb $ pure talkToV

  mkObject cafeToiletDoor $ do
    objectTemplate
    setOName "toilet door"
    setOImage $ do
           isOpen ← getState toiletDoorOpenS
           pure $ Just $ if isOpen then toiletDoorOpenImg else toiletDoorClosedImg
    addOInterceptor [lookAtV] $ do
      playerSayAndThen "There is a scribbled note stuck on its surface." $
        addHotspot note [ Tuple 437.0 159.0
                        , Tuple 456.0 159.0
                        , Tuple 460.0 178.0
                        , Tuple 437.0 178.0
                        , Tuple 437.0 159.0
                        ] -- note should only be here when door is closed..
    addOInterceptor [walkToV] $ do
      isOpen ← getState toiletDoorOpenS
      isLightOn ← getState toiletLightOnS
      if not isOpen then playerSay "The door is closed"
        else if isLightOn then moveTo toiletR
        else moveTo toiletDarknessR
    addOInterceptor [openV] $ do
      hasKey ← hasInInventory key
      isOpen ← getState toiletDoorOpenS
      if isOpen then playerSay "The door is already open"
        else if hasKey then setState toiletDoorOpenS true
        else playerSay "The door is locked"
    addOInterceptor [closeV] $ do
      isOpen ← getState toiletDoorOpenS
      if isOpen then setState toiletDoorOpenS false
        else playerSay "The door is already closed"
    setOSuggestedVerb $ do
      isOpen ← getState toiletDoorOpenS
      pure $ if isOpen then closeV else openV


  mkObject cafeOutsideDoor $ do
    objectTemplate
    setOName "outside door"
    addOInterceptor [walkToV] $ do
      needPayCoffee ← getState needPayCoffeeS
      hasKey        ← hasInInventory key
      costumeWorn   ← getState costumeWornS
      if needPayCoffee && hasKey then do
            bennySayAndThen "Hey! You've got my key and haven't paid for the coffee."
                   (bennySay "Do I look like a chump?")
        else if needPayCoffee then bennySay "Just waidda minute here, Mister, Sneaking out without paying, are you?"
        else if hasKey        then bennySay "Just where you think you're going with the toilet key? You a thief?"
        else if costumeWorn   then playerSayAndThen "Now I'm in costume, I can save the day!" setGameOver
        else  moveTo streetR

  mkObject coffee $ do
    objectTemplate
    setOName "coffee"
    setOImage $ pure $ Just coffeeImg
    setOInventoryImage $ pure $ Just coffeeIImg
    addOInterceptor [pickUpV] $ addUniqueToInventory coffee
    addOInterceptor [lookAtV] $ playerSay "It smells delicious."

  mkObject key $ do
    objectTemplate
    setOName "key"
    setOImage $ pure $ Just keyImg
    setOInventoryImage $ pure $ Just keyIImg
    addOInterceptor [pickUpV] $ addUniqueToInventory key
    addOInterceptor [lookAtV] $ playerSay "Your SUPRA PERCEPTIVE senses detect nothing of consequence about the toilet key." -- text could change depening on whether it is in inventory
    addOInterceptor2 useV "in" $ \noun → when (noun == cafeToiletDoor) $ do
      setState toiletDoorOpenS true -- TODO just delegate to invokeopen
    addOInterceptor2 giveV "to" $ \noun → when (noun == cafeBenny) $ do
     removeFromInventory key
     bennySay "I'm impressed!"

  mkObject note $ do
    objectTemplate
    setOName $ "note"
    addOInterceptor [pickUpV] $ playerSay "No reason to start collecting notes."
    addOInterceptor [lookAtV] $ do
      setState noteReadS true
      playerSay "It says \"Ask benny for the key.\""

  mkRoom cafeR $ do
    setRImage $ pure cafeImg
    setRHotspots $
      [ Tuple cafeFood        [Tuple  712.0 217.0, Tuple  761.0 219.0, Tuple  762.0 245.0, Tuple  708.0 234.0, Tuple  712.0 217.0]
      , Tuple cafeCounter     [Tuple  687.0 194.0, Tuple 1176.0 274.0, Tuple 1186.0 340.0, Tuple  658.0 252.0, Tuple  687.0 194.0]
      , Tuple cafeMenu        [Tuple  721.0  32.0, Tuple  896.0  32.0, Tuple  895.0 128.0, Tuple  725.0 111.0, Tuple  721.0  32.0]
      , Tuple cafeCustomers   [Tuple   11.0 225.0, Tuple  245.0 209.0, Tuple  250.0 304.0, Tuple   22.0 371.0, Tuple   11.0 225.0]
      , Tuple cafeBenny       [Tuple 1034.0 154.0, Tuple 1083.0 154.0, Tuple 1110.0 235.0, Tuple 1039.0 232.0, Tuple 1034.0 154.0]
      , Tuple cafeOutsideDoor [Tuple  218.0 492.0, Tuple  234.0 451.0, Tuple  800.0 452.0, Tuple  814.0 491.0, Tuple  218.0 492.0]
      ]
    setRItems $ [ Tuple cafeToiletDoor (Tuple 350.0 60.0) ] -- position was middle of image, now it's top-left (unless we can get size of image)

--------------------------------------------------------------------------------
-- INSIDE TOILET ---------------------------------------------------------------
--------------------------------------------------------------------------------

  mkObject toiletLightSwitch $ do
    applianceTemplate
    setOName "light switch"
    addOInterceptor [lookAtV] $ playerSay "A notorious ACHIEVEMENT of technological SCIENCE, elegant yet EASY to use."
    addOInterceptor [useV] $ do
      isOn ← getState toiletLightOnS
      if isOn
        then do
          setState toiletLightOnS false
          moveTo toiletDarknessR
        else do
          setState toiletLightOnS true
          moveTo toiletR
    addOInterceptor [turnOnV] $ do
      isOn ← getState toiletLightOnS
      if isOn
        then playerSay "It is already on"
        else do
             setState toiletLightOnS true
             moveTo toiletR
    addOInterceptor [turnOffV] $ do
      isOn ← getState toiletLightOnS
      if isOn
        then do
             setState toiletLightOnS false
             moveTo toiletDarknessR
        else playerSay "It is already off"
    setOSuggestedVerb $ do
      isOn ← getState toiletLightOnS
      pure $ if isOn then turnOffV else turnOnV
    setOInitial $ setState toiletLightOnS false -- not required since initial state was set when defining the state key, but demonstrated how setOIntital could work

  mkObject toiletDoor $ do
    objectTemplate
    setOName "toilet door"
    addOInterceptor [lookAtV] $ playerSay "A door with no OUTSTANDING features."
    addOInterceptor [walkToV] $ moveTo cafeR

  mkObject toiletLavatory $ do
    applianceTemplate
    setOName "lavatory"
    addOInterceptor [lookAtV] $ do
      addToCurrentRoom coin (Tuple 474.0 393.0)
      playerSay "The latest user CIVILLY flushed it after use, but failed to pick up the VALUABLE coin that fell from his pants."
    setOSuggestedVerb $ pure lookAtV

  mkObject coin $ do
    objectTemplate
    setOName "coin"
    setOImage $ pure $ Just coinImg
    setOInventoryImage $ pure $ Just coinIImg
    addOInterceptor [lookAtV] $ playerSay "It's a genuine GOLD QUIDBUCK."
    addOInterceptor [pickUpV] $ addUniqueToInventory coin
    addOInterceptor2 giveV "to" $ \noun → when (noun == cafeBenny) $ do
                                  setState needPayCoffeeS false
                                  removeFromInventory coin
                                  bennySay "Thank you, sir. Come back anytime."

-- treat toilet in darkness as a separate room since simplifies hiding things
  mkRoom toiletDarknessR $ do
    setRImage $ pure toiletDarknessImg
    setRHotspots $
      [ Tuple toiletLightSwitch [Tuple 988.0 180.0, Tuple 1030.0 200.0, Tuple 1023.0 256.0, Tuple 987.0 224.0, Tuple 988.0 180.0]
      , Tuple toiletDoor        [Tuple 218.0 492.0, Tuple  234.0 451.0, Tuple  800.0 452.0, Tuple 814.0 491.0, Tuple 218.0 492.0]
      ]

  mkRoom toiletR $ do
    setRImage $ pure toiletImg
    setRHotspots $
      [ Tuple toiletLightSwitch [Tuple 968.0 160.0, Tuple 1050.0 180.0, Tuple 1043.0 276.0, Tuple 967.0 244.0, Tuple 968.0 160.0]
      , Tuple toiletDoor        [Tuple 218.0 492.0, Tuple  234.0 451.0, Tuple  800.0 452.0, Tuple 814.0 491.0, Tuple 218.0 492.0]
      , Tuple toiletLavatory    [Tuple 478.0 251.0, Tuple  590.0 247.0, Tuple  567.0 414.0, Tuple 519.0 414.0, Tuple 478.0 251.0]
      ]

  mkObject costume $ do
    objectTemplate
    setOName "costume"
    setOInventoryImage $ pure $ Just costumeIImg
    addOInterceptor [lookAtV] $ do
      playerSay "STATE OF THE ART manufacture"
      playerSay "from chemically reinforced 100% COTTON-lastic(tm)."
    addOInterceptor [useV] $ do
      currentRoom ← getCurrentRoom
      if currentRoom  == streetR then do
         playerSayAndThen "In the middle of the street? That would be a PUBLIC SCANDAL"
           (playerSay "to say nothing of revealing your secret identity.")
        else if currentRoom == boothR then do
           playerSay "Lacking Superman's super-speed, you realise that it would be awkward to change in plain view of the passing pedestrians."
        else if currentRoom == cafeR then do
           bennySay "Oi! No monkey business in my establishment."
        else if currentRoom == toiletDarknessR then do
           playerSay "Last time you changed in the dark, you wore the suit inside out!"
        else if currentRoom == toiletR then do
           playerSayAndThen "You quickly remove your street clothes and bundle them up"
             (playerSay "together into an INFRA MINUSCULE pack ready for easy transportation.")
           setState costumeWornS true
           removeFromInventory costume
           addToInventory clothes
        else pure unit

  mkObject clothes $ do
    objectTemplate
    setOName "clothes"
    setOInventoryImage $ pure $ Just clothesIImg
    addOInterceptor [lookAtV] $ playerSay "Perfectly ORDINARY-LOOKING street clothes for a NOBODY."
    addOInterceptor [useV]    $ playerSay "I've just taken them off!"



  setSInitial $ do
    addToInventory costume
    playerSayAndThen "There's some MADMAN attacking the population in Granary Park!"
      (playerSay "I must change into my Captain FATE costume fast...!")
  -- game over hook to print stuff out?
