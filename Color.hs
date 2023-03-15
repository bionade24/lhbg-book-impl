import Data.Word
import Data.Maybe

-- | A data type representing colours
data Colour
  = RGB Word8 Word8 Word8

getBluePart :: Colour -> Word8
getBluePart colour =
  case colour of
    RGB _ _ blue -> blue

data Brightness
  = Dark
  | Bright

data EightColour
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColour
  = AnsiColour Brightness EightColour

ansiColourToVGA :: AnsiColour -> Colour
ansiColourToVGA ansicolour =
  case ansicolour of
    AnsiColour Dark Black ->
      RGB 0 0 0
    AnsiColour Bright Black ->
      RGB 85 85 85
    AnsiColour Dark Red ->
      RGB 170 0 0
    AnsiColour Bright Red ->
      RGB 255 85 85
    -- and so on

isBright :: AnsiColour -> Bool
isBright ansicolour =
  case ansicolour of
    AnsiColour Bright _ ->
      True
    _ ->
      False

ansiToUbuntu :: AnsiColour -> Colour
ansiToUbuntu ansiColour =
  case ansiColour of
    AnsiColour brightness colour ->
      case brightness of
        Dark ->
          case colour of
             Black -> RGB 1 1 1
             Red -> RGB 222 56 43
             Green -> RGB 57 181 74
             Yellow -> RGB 173 173 39
             Blue -> RGB 73 46 225
             Magenta -> RGB 211 56 211
             Cyan -> RGB 51 187 200
             White -> RGB 203 204 205
        Bright ->
          case colour of
           Black -> RGB 128 128 128
           Red -> RGB 252 57 31
           Green -> RGB 49 231 34
           Yellow -> RGB 234 236 35
           Blue -> RGB 88 51 255
           Magenta -> RGB 249 53 248
           Cyan -> RGB 20 240 240
           White -> RGB 233 235 235

isEmpty :: [a] -> Bool
--isEmpty list =
--  case listToMaybe list of
--    Nothing -> True
--    _ -> False

isEmpty list =
  case length list of
    0 -> True
    _ -> False
