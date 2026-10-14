library(tidyverse)

haunted <- haunted_places

glimpse(haunted)


# ---- Cleaning: drop duplicate rows ---------------------------
# distinct() keeps only the unique rows and throws away repeats.

# nrow() counts rows, so this tells you how many got dropped.
nrow(haunted)

haunted <- haunted %>%
  distinct()

nrow(haunted)   



# Derivning entity_type


haunted <- haunted %>%
  mutate(
    entity_type = case_when(
      str_detect(description, regex("poltergeist", ignore_case = TRUE))                                      ~ "poltergeist",
      str_detect(description, regex("\\bdemon|\\bdevil\\b|satanic", ignore_case = TRUE))                     ~ "demon",
      str_detect(description, regex("\\bwitch", ignore_case = TRUE))                                         ~ "witch",
      str_detect(description, regex("shadow (figure|person|man|people)|dark figure", ignore_case = TRUE))     ~ "shadow figure",
      str_detect(description, regex("\\bphantom", ignore_case = TRUE))                                       ~ "phantom",
      str_detect(description, regex("\\borb\\b|\\borbs\\b", ignore_case = TRUE))                             ~ "orb",
      str_detect(description, regex("\\bapparition", ignore_case = TRUE))                                    ~ "apparition",
      str_detect(description, regex("\\bspirit", ignore_case = TRUE))                                        ~ "spirit",
      str_detect(description, regex("\\bghost", ignore_case = TRUE))                                          ~ "ghost",
      TRUE                                                                                                    ~ "unspecified"
    ),
    entity_type = factor(entity_type)
  )

haunted %>%
  count(entity_type, sort = TRUE)

haunted %>%
  filter(entity_type == "poltergeist") %>%
  select(state, location, description) %>%
  head(3)


# Deriving year columns - 4-digit years only

# The pattern "\\b(1[5-9]\\d\\d|20[0-2]\\d)\\b" means:
#   1[5-9]\\d\\d  -> 1500-1999 (1887, 1963)
#   20[0-2]\\d    -> 2000-2029 (2003)
#   \\b ... \\b   -> whole numbers only, so a street address like
#                    "Route 1800A" or a 5-digit number won't match


year_pattern <- "\\b(1[5-9]\\d\\d|20[0-2]\\d)\\b"

haunted <- haunted %>%
  mutate(
    year = as.integer(str_extract(description, year_pattern)),

    # TRUE/FALSE helper: did this row have a year at all?
    has_year = !is.na(year),

    # Decade, by rounding down to the nearest 10. Stays NA when
    # year is NA
    decade = floor(year / 10) * 10
  )

haunted %>%
  count(has_year)

# Oldest and newest years mentioned. na.rm = TRUE tells R to skip
# the empty rows instead of returning NA.
haunted %>%
  summarise(
    earliest = min(year, na.rm = TRUE),
    latest   = max(year, na.rm = TRUE),
    n_years  = sum(has_year)
  )


# ==============================================================
# PART 3: setting -- ONE column, same style as entity_type
# ==============================================================
# The setting is often named in the `location` column rather than
# in the description ("Railroad Crossing", "University Chapel"),
# so we search BOTH. str_c() glues strings together (Ch. 14).
#
# coalesce() swaps NA for "" first. Without it, str_c() would turn
# the whole row into NA whenever location is missing (3 rows here).

haunted <- haunted %>%
  mutate(
    setting_text = str_c(coalesce(location, ""), " ", description)
  )

# Same case_when() idea as entity_type: first TRUE wins, so the
# ORDER sets the priority. Specific venues go first; broad words
# like "house", "road" and "building" go last, because they show
# up inside other kinds of stories ("the house next to the old
# mill") and would otherwise grab those rows first.

haunted <- haunted %>%
  mutate(
    setting = case_when(
      str_detect(setting_text, regex("cemeter|graveyard|\\bgraves?\\b|\\btomb|mausoleum|\\bcrypt\\b|burial ground", ignore_case = TRUE)) ~ "cemetery",
      str_detect(setting_text, regex("hospital|asylum|sanatorium|sanitarium|infirmary|morgue|nursing home", ignore_case = TRUE))        ~ "hospital",
      str_detect(setting_text, regex("\\bprison\\b|\\bjail\\b|penitentiary|\\bdungeon\\b", ignore_case = TRUE))                        ~ "prison",
      str_detect(setting_text, regex("church|chapel|cathedral|convent|monastery|\\babbey\\b", ignore_case = TRUE))                      ~ "church",
      str_detect(setting_text, regex("school|universit|college|campus|\\bdorm|classroom|academy|fraternit|sororit", ignore_case = TRUE)) ~ "school",
      str_detect(setting_text, regex("theat(er|re)|\\bopera\\b|auditorium|playhouse", ignore_case = TRUE))                              ~ "theater",
      str_detect(setting_text, regex("\\bhotel\\b|\\binn\\b|motel|\\blodge\\b|boarding house", ignore_case = TRUE))                     ~ "hotel",
      str_detect(setting_text, regex("\\bbar\\b|tavern|saloon|\\bpub\\b|restaurant|\\bcafe\\b|diner", ignore_case = TRUE))              ~ "restaurant or bar",
      str_detect(setting_text, regex("factory|\\bmill\\b|\\bmine\\b|\\bmines\\b|warehouse|foundry|quarry", ignore_case = TRUE))         ~ "factory or mine",
      str_detect(setting_text, regex("\\bfort\\b|barracks|battlefield|arsenal|military base", ignore_case = TRUE))                      ~ "military site",
      str_detect(setting_text, regex("library|courthouse|museum|city hall|town hall", ignore_case = TRUE))                              ~ "civic building",
      str_detect(setting_text, regex("lighthouse|\\blake\\b|\\briver\\b|\\bcreek\\b|beach|\\bocean\\b|\\bpond\\b|harbor", ignore_case = TRUE)) ~ "water or lighthouse",
      str_detect(setting_text, regex("bridge|tunnel|railroad|railway|\\btracks\\b|trestle|overpass", ignore_case = TRUE))               ~ "bridge or railroad",
      str_detect(setting_text, regex("\\bhouse\\b|\\bhome\\b|mansion|\\bmanor\\b|\\bfarm|cabin|castle|apartment|cottage|plantation", ignore_case = TRUE)) ~ "house",
      str_detect(setting_text, regex("\\bpark\\b|forest|\\bwoods\\b|\\btrail\\b|mountain|\\bcave\\b|\\bhill\\b|canyon|swamp|\\bisland\\b", ignore_case = TRUE)) ~ "outdoors",
      str_detect(setting_text, regex("\\broad\\b|\\bstreet\\b|highway|\\bavenue\\b|\\blane\\b|\\bdrive\\b", ignore_case = TRUE))        ~ "road",
      str_detect(setting_text, regex("\\bstore\\b|\\bshop\\b|\\bmall\\b|\\boffice\\b|\\bbank\\b|\\bbuilding\\b", ignore_case = TRUE))   ~ "store or office",
      TRUE                                                                                                                              ~ "unspecified"
    ),
    setting = factor(setting)
  ) %>%
  # setting_text was only scratch work, so drop it (Ch. 5)
  select(-setting_text)

# Check it.
haunted %>%
  count(setting, sort = TRUE)

# Spot-check a category you're unsure about.
haunted %>%
  filter(setting == "road") %>%
  select(state, location, description) %>%
  head(3)

# Two categorical columns at once: count() takes more than one.
haunted %>%
  count(setting, entity_type, sort = TRUE) %>%
  head(10)


# ==============================================================
# PART 4: phenomena_type -- what people experience
# ==============================================================
# Four kinds of experience:
#   physical -- things move on their own (doors, objects, lights)
#   tactile  -- felt on the body (touched, cold spots, watched)
#   auditory -- heard (footsteps, voices, screams, music)
#   visual   -- seen (figures, lights, orbs, mist)
#
# Same case_when() pattern, and the ORDER is doing real work here.
# Most descriptions mention more than one kind, so the order
# decides which one wins. It runs from most distinctive to most
# generic: a slamming door is a specific claim, while "was seen"
# shows up in nearly every story, so visual goes last.

haunted <- haunted %>%
  mutate(
    phenomena_type = case_when(
      str_detect(description, regex("door\\w* (slam|open|clos|shut|lock)|slamm|object\\w* (mov|thrown|fl)|\\bmoved\\b|\\bmoving\\b|levitat|thrown|furniture|rocking chair|flicker|turn(s|ed)? (on|off)", ignore_case = TRUE)) ~ "physical",
      str_detect(description, regex("\\btouch|\\bpush|\\bpull|grab|tugg|scratch|slap|cold spot|cold (air|breeze|draft|chill)|\\bchills?\\b|\\bicy\\b|temperature|goosebump|hair stand|being watched|\\bpresence\\b", ignore_case = TRUE)) ~ "tactile",
      str_detect(description, regex("footstep|\\bvoices?\\b|whisper|scream|\\bcry|cries|\\bsob|moan|wail|laugh|giggl|knock|\\bbang|thump|\\bnoises?\\b|\\bmusic\\b|\\bpiano\\b|\\bsounds?\\b|\\bheard\\b|\\bhear\\b|rattl|chain", ignore_case = TRUE)) ~ "auditory",
      str_detect(description, regex("\\bseen\\b|\\bsee\\b|\\bsight|appear|\\bfigure\\b|\\bvisible\\b|apparition|\\blights?\\b|\\borbs?\\b|glow|\\bmist\\b|\\bfog\\b|shadow|\\bwitness|photograph", ignore_case = TRUE)) ~ "visual",
      TRUE                                                                                                                                                                                                                       ~ "unspecified"
    ),
    phenomena_type = factor(phenomena_type)
  )

haunted %>%
  count(phenomena_type, sort = TRUE)


# ==============================================================
# PART 5: death_cause -- how the person died
# ==============================================================
# Most descriptions never say, so "unspecified" is the biggest
# group by far. That is the honest answer, not a gap to fill in.
#
# ORDER matters a lot here. "murder" has to go LAST among the
# violent causes because its pattern includes the bare word
# "killed", which also appears in hangings, war deaths, and
# accidents. Putting it first would pull those rows away from
# their real cause. Specific phrases go first.

haunted <- haunted %>%
  mutate(
    death_cause = case_when(
      str_detect(description, regex("suicide|hung (him|her|them)self|hanged (him|her|them)self|killed (him|her|them)self|shot (him|her|them)self|took (his|her|their) own life|jumped (off|from)", ignore_case = TRUE)) ~ "suicide",
      str_detect(description, regex("\\bhanged\\b|\\bhung\\b|lynch|execut|gallows|\\bnoose\\b|electric chair|firing squad", ignore_case = TRUE))                                                                        ~ "execution",
      str_detect(description, regex("drown", ignore_case = TRUE))                                                                                                                                                       ~ "drowning",
      str_detect(description, regex("childbirth|giving birth|died in labor", ignore_case = TRUE))                                                                                                                       ~ "childbirth",
      str_detect(description, regex("disease|epidemic|plague|tubercul|cholera|yellow fever|typhoid|smallpox|influenza|consumption|illness|\\bcancer\\b", ignore_case = TRUE))                                            ~ "illness",
      str_detect(description, regex("\\bbattle\\b|killed in action|civil war|revolutionary war|\\bwar\\b", ignore_case = TRUE))                                                                                          ~ "war",
      str_detect(description, regex("burn(ed|t|ing)|\\bfire\\b|\\bblaze\\b|\\bflames\\b|arson", ignore_case = TRUE))                                                                                                     ~ "fire",
      str_detect(description, regex("accident|\\bcrash|hit by|struck by|run over|collision|electrocut|explosion|collaps|cave-?in|fell (from|off|down|to)|derail", ignore_case = TRUE))                                   ~ "accident",
      str_detect(description, regex("murder|stabb|strangl|\\bshot\\b|shooting|beaten to death|massacre|slaughter|\\bslain\\b|\\baxe\\b|homicide|\\bkilled\\b", ignore_case = TRUE))                                      ~ "murder",
      TRUE                                                                                                                                                                                                              ~ "unspecified"
    ),
    death_cause = factor(death_cause)
  )

haunted %>%
  count(death_cause, sort = TRUE)

# Spot-check: does "execution" really look like executions?
haunted %>%
  filter(death_cause == "execution") %>%
  select(location, description) %>%
  head(3)


# ==============================================================
# PART 6: time_of_day -- when the haunting happens
# ==============================================================
# ORDER here goes from the NARROWEST time window to the BROADEST.
# "midnight" is one specific hour, while "night" covers every dark
# hour, so night goes last and acts as the catch-all. A story that
# says "at dusk and again at night" lands in evening, the more
# specific claim.

haunted <- haunted %>%
  mutate(
    time_of_day = case_when(
      str_detect(description, regex("midnight|witching hour|12:00|twelve o.?clock|\\b12 ?a\\.?m", ignore_case = TRUE))             ~ "midnight",
      str_detect(description, regex("\\bdawn\\b|daybreak|sunrise|early morning|\\bmorning\\b", ignore_case = TRUE))                 ~ "dawn",
      str_detect(description, regex("\\bevening\\b|\\bdusk\\b|sunset|sundown|twilight", ignore_case = TRUE))                        ~ "evening",
      str_detect(description, regex("daytime|during the day|\\bafternoon\\b|\\bnoon\\b|broad daylight|\\bdaylight\\b|midday", ignore_case = TRUE)) ~ "daytime",
      str_detect(description, regex("at night|\\bnightly\\b|after dark|night ?time|\\bnights?\\b|\\b(1[0-2]|[1-9]) ?p\\.?m", ignore_case = TRUE)) ~ "night",
      TRUE                                                                                                                          ~ "unspecified"
    ),
    time_of_day = factor(time_of_day)
  )

haunted %>%
  count(time_of_day, sort = TRUE)


# ==============================================================
# PART 7: division -- the nine U.S. Census divisions
# ==============================================================
# This one is NOT a str_detect() problem. The state is already in
# its own column, so instead we build a small lookup table and
# JOIN it on (Ch. 13 Relational data). That is the right tool
# whenever you want to attach extra facts to a key you already
# have.
#
# Base R ships three handy vectors -- state.name, state.region and
# state.division -- so you don't type out 50 states by hand.
# tibble() builds the lookup table from two of them (Ch. 10).

state_divisions <- tibble(
  state    = state.name,
  division = as.character(state.division)
)

# Base R's list has no Washington DC, but your data has 16 rows of
# it. add_row() appends it so those rows don't come out NA. The
# Census counts DC as South Atlantic.
state_divisions <- state_divisions %>%
  add_row(state = "Washington DC", division = "South Atlantic")

# left_join() keeps every row of `haunted` and adds the matching
# division (Ch. 13). "left" means: never drop a haunted place,
# even if its state somehow isn't in the lookup table.
haunted <- haunted %>%
  left_join(state_divisions, by = "state") %>%
  mutate(division = factor(division))

# ALWAYS check a join for unmatched rows. If this is more than 0,
# a state name in your data doesn't match the lookup table.
haunted %>%
  filter(is.na(division)) %>%
  count(state)

haunted %>%
  count(division, sort = TRUE)


# ==============================================================
# PART 8: look at it
# ==============================================================
# fct_infreq() reorders bars from most to least common (Ch. 15).
# geom_bar() counts the rows for you (Ch. 3).

haunted %>%
  mutate(entity_type = fct_infreq(entity_type)) %>%
  ggplot(aes(x = entity_type)) +
  geom_bar() +
  labs(
    title = "Haunted places by entity type",
    x = "Entity type",
    y = "Number of places"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Years over time. filter() drops the empty rows first so ggplot
# doesn't warn about them. geom_histogram() buckets the years.
haunted %>%
  filter(has_year) %>%
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Years mentioned in haunted place descriptions",
    x = "Year",
    y = "Number of places"
  )

# Settings, most to least common.
haunted %>%
  mutate(setting = fct_infreq(setting)) %>%
  ggplot(aes(y = setting)) +
  geom_bar() +
  labs(
    title = "Haunted places by setting",
    x = "Number of places",
    y = "Setting"
  )

# Phenomena split by setting. fill = puts one colour per
# phenomena type inside each setting's bar (Ch. 3).
haunted %>%
  mutate(setting = fct_infreq(setting)) %>%
  ggplot(aes(y = setting, fill = phenomena_type)) +
  geom_bar() +
  labs(
    title = "What people experience, by setting",
    x = "Number of places",
    y = "Setting",
    fill = "Phenomena"
  )

# Places per division.
haunted %>%
  mutate(division = fct_infreq(division)) %>%
  ggplot(aes(y = division)) +
  geom_bar() +
  labs(
    title = "Haunted places by Census division",
    x = "Number of places",
    y = "Division"
  )

# Does time of day differ by division? position = "fill" turns
# each bar into proportions instead of raw counts (Ch. 3), which
# is what you want when the divisions have different totals.
haunted %>%
  filter(time_of_day != "unspecified") %>%
  ggplot(aes(y = division, fill = time_of_day)) +
  geom_bar(position = "fill") +
  labs(
    title = "When hauntings happen, by division",
    x = "Share of places that name a time",
    y = "Division",
    fill = "Time of day"
  )


# ---- Save ----------------------------------------------------
# Writes a new file; your original CSV is untouched.

write_csv(haunted, "haunted_places_clean.csv")
