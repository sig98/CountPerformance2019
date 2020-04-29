#install.packages('tidyverse')
library(tidyverse)
#install.packages('dplyr')
library(tidyr)
library(dplyr)

EventsData <- read.csv('Events.csv', stringsAsFactors = F)
EventsData$count <- paste(EventsData$balls, EventsData$strikes, sep = "-")
EventList <- unique(EventsData$events)
EventSample <- EventsData %>%
  sample_n(10)
PlayerList <- read.csv('PlayerList.csv', stringsAsFactors = F)

ZeroZero <- filter(EventsData, count == "0-0")
ZeroOne <- filter(EventsData, count == "0-1")
ZeroTwo <- filter(EventsData, count == "0-2")
OneTwo <- filter(EventsData, count == "1-2")
TwoTwo <- filter(EventsData, count == "2-2")
OneZero <- filter(EventsData, count == "1-0")
TwoZero <- filter(EventsData, count == "2-0")
ThreeZero <- filter(EventsData, count == "3-0")
OneOne <- filter(EventsData, count == "1-1")
TwoOne <- filter(EventsData, count == "2-1")
ThreeOne <- filter(EventsData, count == "3-1")
ThreeTwo <- filter(EventsData, count == "3-2")

ZeroZeroStats <- ZeroZero %>%
  group_by(player_name) %>%
    summarise(
      PA = n(),
      H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
      Single = sum(events == 'single'),
      Double = sum(events == 'double'),
      Triple = sum(events == 'triple'),
      HomeRun = sum(events == 'home_run'),
      Strikeout = sum(events == 'strikeout'),
      Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
      BB = sum(events == 'walk'),
      AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
                 (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
                 (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
                 (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
      mutate(
        AVG = ((H/AB)),
        StrikeoutP = (Strikeout/AB),
        OBP = ((H+BB)/PA),
        SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
        BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
      )
write.csv(ZeroZeroStats,"ZeroZeroStats.csv",row.names = F)

ZeroOneStats <- ZeroOne %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play')+ (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(ZeroOneStats,"ZeroOneStats.csv",row.names = F)

ZeroTwoStats <- ZeroTwo %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    StrikeoutP = (Strikeout/AB),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(ZeroTwoStats,"ZeroTwoStats.csv",row.names = F)

OneZeroStats <- OneZero %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(OneZeroStats,"OneZeroStats.csv",row.names = F)

TwoZeroStats <- TwoZero %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(TwoZeroStats,"TwoZeroStats.csv",row.names = F)

OneTwoStats <- OneTwo %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(OneTwoStats,"OneTwoStats.csv",row.names = F)

TwoOneStats <- TwoOne %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(TwoOneStats,"TwoOneStats.csv",row.names = F)

TwoTwoStats <- TwoTwo %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(TwoTwoStats,"TwoTwoStats.csv",row.names = F)

OneOneStats <- OneOne %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    StrikeoutP = (Strikeout/AB),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(OneOneStats,"OneOneStats.csv",row.names = F)

ThreeZeroStats <- ThreeZero %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    StrikeoutP = (Strikeout/AB),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(ThreeZeroStats,"ThreeZeroStats.csv",row.names = F)

ThreeOneStats <- ThreeOne %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    StrikeoutP = (Strikeout/AB),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(ThreeOneStats,"ThreeOneStats.csv",row.names = F)

ThreeTwoStats <- ThreeTwo %>%
  group_by(player_name) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    StrikeoutP = (Strikeout/AB),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(ThreeTwoStats,"ThreeTwoStats.csv",row.names = F)

CountStats <- EventsData %>%
  group_by(count) %>%
  summarise(
    PA = n(),
    H = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run')),
    Single = sum(events == 'single'),
    Double = sum(events == 'double'),
    Triple = sum(events == 'triple'),
    HomeRun = sum(events == 'home_run'),
    Strikeout = sum(events == 'strikeout'),
    Sacrifice = sum((events == 'sac_fly')+(events == 'sac_bunt')+(events == 'sac_bunt_double_play')+(events == 'sac_fly_double_play')),
    BB = sum(events == 'walk'),
    AB = sum((events == 'double')+ (events =='triple') +(events == 'single')+ (events =='home_run') + 
               (events =='strikeout_double_play') + (events =='force_out') + (events =='other_out') +
               (events =='fielders_choice_out') + (events =='field_out') + (events == 'field_error') + 
               (events == 'double_play') + (events == 'grounded_into_double_play') + (events == 'strikeout')
    )) %>%
  mutate(
    AVG = ((H/AB)),
    StrikeoutP = (Strikeout/AB),
    OBP = ((H+BB)/PA),
    SLG = ((((Single)+(Double *2)+(Triple*3)+(HomeRun*4))/AB)),
    BABIP = ((H-HomeRun)/((AB-HomeRun-Strikeout+Sacrifice)))
  )
write.csv(CountStats,"CountStats.csv",row.names = F)
