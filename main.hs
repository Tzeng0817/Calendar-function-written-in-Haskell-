import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Char
year = 2019
month = 11
months = ["JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER"]
mnth = months!!(month-1)
monthDate = getDaysInMonth year month
startAt = snd(monthDate)
totalDays = fst(monthDate)


getDaysInMonth year month=(nDays,sDay)
                      where nDays = gregorianMonthLength year month
                            sDay= digitToInt(last(showWeekDate (fromGregorian year month 01)))

cells = (replicate n1 "")
                          ++ [ show d | d <- [1..lastday] ]
                          ++ (replicate n2 "")
          where lastday = totalDays
                n1=(startAt-1)                       -- n1:
                n2=(7-(totalDays-(29-startAt)))      -- n2: days left in the last week

chunksOf7 []=[]
chunksOf7 (a:b:c:d:e:f:g:rest) = [ [a,b,c,d,e,f,g] ] ++ chunksOf7 rest


pad n str = str ++ (replicate (n-(length str)) ' ')
renderCell c = pad 3 c ++ "|"
renderChunk cells =  (concat $ map renderCell cells) ++ "\n"
printLine = "\n" ++ concat ["----" | r <- [1..7]]++"\n"
monthLine = concat [" " ++ [r]| r <- mnth]
monthLinePlus = "** " ++ monthLine ++ "  **"
dayLine="Sun|Mon|Tue|Wed|Thu|Fri|Sat"
yearLine = "      **  " ++ show year ++ "  **"
heading = concat[printLine,"Year: ",show year,printLine,monthLine,printLine,dayLine,printLine]

headingPlus = concat[printLine, yearLine, printLine, monthLinePlus, printLine, dayLine, printLine]


renderCalendarBody cells =  heading ++ concat (map renderChunk chunks)
                         where chunks = chunksOf7 cells


cell newYear newMonth = (replicate n1 "")
                                    ++ [show d | d <- [1..lastday] ]
                                    ++ (replicate n2 "")
                                    where lastday = fst(getDaysInMonth newYear newMonth)
                                          n1 = if n1 == 7
                                               then 0
                                               else n1
                                             where n1 = (snd(getDaysInMonth newYear newMonth))
                                          n2 = if (fst(getDaysInMonth newYear newMonth)+(snd(getDaysInMonth newYear newMonth))) > 35
                                               then (42 - fst(getDaysInMonth newYear newMonth) - snd(getDaysInMonth newYear newMonth) )
                                               else (35 - fst(getDaysInMonth newYear newMonth) - snd(getDaysInMonth newYear newMonth) )


renderCalendarBodySuperPlus newYear newMonth =  headingPlus ++ concat (map renderChunk chunks)
                                    where chunks = chunksOf7 cellResult
                                                 where cellResult = cell newYear newMonth

renderCalendarBodyPlus newYear newMonth = concat[printLine, "      **  " ++ show newYear ++ "  **", printLine, "   **  " ++ show (months !!(newMonth-1)) ++ "  **", printLine, dayLine, printLine] ++ concat (map renderChunk (chunksOf7(cell newYear newMonth)))
                                        where chunks = chunksOf7 cellResult
                                                     where cellResult = cell newYear newMonth

printHeading = putStrLn (headingPlus ++ "\n")

calendar newYear newMonth = putStrLn (renderCalendarBodyPlus newYear newMonth)


main= do putStrLn $ renderCalendarBody cells
