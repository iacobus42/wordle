library(tidyverse)
library(parallel)

source("~/wordle/word-list.R")

play_game <- function(answer, guess, candidates = allowed_answers) {
  first_guess <- guess
  state <- list(
    knowledge = list(
      current_known = c("\\D", "\\D", "\\D", "\\D", "\\D"),
      unknown_location = c(),
      not_in_word = c()
    ),
    candidates = candidates
  )

  n_rounds <- 1
  while(length(state$candidates) > 1) {
    state <- play_round(state, answer, guess)
    guess <- sample(state$candidates, 1)
    n_rounds <- n_rounds + 1
  }

  return(tibble(
    first_guess = first_guess,
    answer = answer,
    n_rounds = n_rounds
  ))
}

play_round <- function(state, answer, guess) {
  new_knowledge <- update_knowledge(state$knowledge, answer, guess)
  new_candidates <- reduce_non_matches(state$candidates, new_knowledge)
  return(
    list(
      knowledge = new_knowledge,
      candidates = new_candidates
    )
  )
}

update_knowledge <- function(knowledge, answer, guess) {
  current_known <- knowledge$current_known
  unknown_location <- knowledge$unknown_location
  not_in_word <- knowledge$not_in_word
  for (index in 1:5) {
    in_word <- stringr::str_detect(answer, stringr::str_sub(guess, index, index))
    if (in_word) {
      correct_position <- stringr::str_sub(guess, index, index) == stringr::str_sub(answer, index, index)
      if (correct_position) {
        current_known[index] <- stringr::str_sub(guess, index, index)
      } else {
        unknown_location <- c(unknown_location,
                              stringr::str_sub(guess, index, index))
      }
    } else {
      not_in_word <- c(not_in_word,
                       stringr::str_sub(guess, index, index))
    }
  }

  knowledge$current_known <- current_known
  knowledge$unknown_location <- unknown_location
  knowledge$not_in_word <- not_in_word

  return(knowledge)
}

reduce_non_matches <- function(candidates, knowledge) {
  current_known <- knowledge$current_known
  unknown_location <- knowledge$unknown_location
  not_in_word <- knowledge$not_in_word

  # filter by known index
  if (any(current_known != "\\D")) {
    candidates <- candidates[stringr::str_detect(candidates, paste(current_known, collapse = ""))]
  }

  # remove known not in word
  ## first we have to check because the game can give confusing results when the
  ## letter is in the word and known - extra uses of the letter may return as
  ## black
  if (length(not_in_word) > 0) {
    not_in_word <- unique(not_in_word)
    not_in_word <- not_in_word[!(not_in_word %in% current_known)]
    not_in_word <- not_in_word[!(not_in_word %in% unknown_location)]

    not_in_word_string <- paste(c("[", paste(not_in_word, collapse = ""), "]"), collapse = "")
    candidates <- candidates[!stringr::str_detect(candidates, not_in_word_string)]
  }

  # remove if missing known but unknown location
  if (length(unknown_location) > 0) {
    for (i in length(unknown_location)) {
      candidates <- candidates[stringr::str_detect(candidates, unknown_location[[i]])]
    }
  }

  return(candidates)
}

cluster <- makeCluster(12)
clusterEvalQ(cluster, library(tidyverse))
clusterExport(cluster,
              c("play_game", "play_round", "update_knowledge",
                "reduce_non_matches"))

# allowed_guesses only contains guesses that are allowed but that are not 
# answers, expand that here
allowed_guesses <- c(allowed_guesses, allowed_answers)

# loop over all the allowed answers (2,315) and evaluate, in parallel using
# cluster `cluster`, the 12,972 allowed guesses
i <- 1
results <- vector("list", length(allowed_guesses))
p <- progress::progress_bar$new(
  total = length(allowed_guesses),
  format = "[:bar] :percent in :elapsed with :eta remaining"
)
for (guess in allowed_guesses) {
  results[[i]] <- parLapply(
    cluster,
    allowed_answers,
    play_game,
    guess = guess,
    candidates = allowed_answers) %>%
    bind_rows()
  i <- i + 1
  p$tick()
}

results <- results %>%
  bind_rows()

results %>%
  mutate(
    answer = as.numeric(as.factor(answer)),
    first_guess = as.numeric(as.factor(first_guess))
  ) %>%
  filter(n_rounds <= 6) %>%
  ggplot(aes(x = answer, y = first_guess, fill = n_rounds)) +
  geom_tile() +
  labs(x = "Answer", y = "First Guess", color = "Average Rounds to Solve") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("~/Desktop/wordle-graph-4x6.png",
       width = 6,
       height = 4,
       units = "in",
       bg = "white")

lm(n_rounds ~ first_guess,
   data = results %>%
     filter(as.numeric(as.factor(results$answer)) <= 10)
)
results

by_answer <- results %>%
  group_by(answer) %>%
  summarize(
    mean_rounds = mean(n_rounds),
    win_prob = mean(n_rounds <= 6)
  )

# hardest words
by_answer %>%
  top_n(5, mean_rounds) %>%
  arrange(desc(mean_rounds)) %>%
  select(answer, mean_rounds)

by_answer %>%
  top_n(5, desc(win_prob)) %>%
  arrange(win_prob) %>%
  select(answer, win_prob)

# easiest words
by_answer %>%
  top_n(5, desc(mean_rounds)) %>%
  arrange(mean_rounds) %>%
  select(answer, mean_rounds)

by_answer %>%
  top_n(5, win_prob) %>%
  arrange(desc(win_prob)) %>%
  select(answer, win_prob)

# by guesses
by_guess <- results %>%
  group_by(first_guess) %>%
  summarize(
    mean_rounds = mean(n_rounds),
    win_prob = mean(n_rounds <= 6)
  )

# worst guess
by_guess %>%
  top_n(5, mean_rounds) %>%
  arrange(desc(mean_rounds)) %>%
  select(first_guess, mean_rounds)

by_guess %>%
  top_n(5, desc(win_prob)) %>%
  arrange(win_prob) %>%
  select(first_guess, win_prob)

# best guess
by_guess %>%
  top_n(5, desc(mean_rounds)) %>%
  arrange(mean_rounds) %>%
  select(first_guess, mean_rounds)

by_guess %>%
  top_n(5, win_prob) %>%
  arrange(desc(win_prob)) %>%
  select(first_guess, win_prob)

results %>%
  sample_frac(0.01) %>%
  ggplot(aes(x = answer, y = first_guess, fill = n_rounds)) +
  geom_tile()
