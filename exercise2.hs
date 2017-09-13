module UsePictures where
import Pictures

-- Assignment 2.4b

-- Flip ASCII horse along vertical axis so it faces the correct side as the actual picture
leftHorse = flipV horse

whiteHorse :: Picture
whiteHorse = invertColour leftHorse

rotateHorse :: Picture
rotateHorse = flipH (flipV leftHorse)

-- 1st image
horse1 = (leftHorse `beside` whiteHorse) `above` (whiteHorse `beside` leftHorse)

-- 2nd image
horse2 = (leftHorse `beside` whiteHorse) `above` ((flipV whiteHorse) `beside` horse)

-- 3rd image
horse3 = (leftHorse `beside` whiteHorse) `above` ((invertColour rotateHorse) `beside` rotateHorse)


-- Assignment 2.5
evilTwin :: Picture -> Picture
evilTwin pic = invertColour (flipV pic)

test_evilTwin :: Bool
test_evilTwin = evilTwin (evilTwin horse) == horse
