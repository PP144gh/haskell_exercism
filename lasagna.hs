-- LUCIANS LUCIOUS LASAGNA --
type Minutes = Integer
type Layers = Integer


expectedMinutesInOven :: Minutes
expectedMinutesInOven = 40;

preparationTimeInMinutes :: Layers -> Minutes
-- preparationTimeInMinutes = (* 2)
preparationTimeInMinutes layers = 2*layers


elapsedTimeInMinutes :: Layers -> Minutes -> Minutes
-- elapsedTimeInMinutes = (+) . preparationTimeInMinutes
elapsedTimeInMinutes layers minutesoven = (preparationTimeInMinutes layers) + minutesoven

---------------------------
