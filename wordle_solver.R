library(tidyverse)
library(parallel)

source("~/Desktop/word-list.R")

allowed_guesses <- c(allowed_guesses, allowed_answers)

play_game <- function(answer, guess, candidates = allowed_answers, guesses = allowed_guesses) {
  first_guess <- guess
  state <- list(
    knowledge = list(
      current_known = c("\\D", "\\D", "\\D", "\\D", "\\D"),
      unknown_location = c(),
      not_in_word = c()
    ),
    candidates = candidates,
    guesses = guesses
  )

  n_rounds <- 1
  while(length(state$candidates) > 1) {
    state <- play_round(state, answer, guess)
    guess <- sample(state$guesses, 1)
    n_rounds <- n_rounds + 1
    print(str(state))
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
  new_guesses <- reduce_non_matches(state$guesses, new_knowledge)
  return(
    list(
      knowledge = new_knowledge,
      candidates = new_candidates,
      guesses = new_guesses
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

play_game_optimally <- function(answer, guess, cl, allowed_guesses = allowed_guesses) {
  first_guess <- guess
  state <- list(
    knowledge = list(
      current_known = c("\\D", "\\D", "\\D", "\\D", "\\D"),
      unknown_location = c(),
      not_in_word = c()
    ),
    candidates = candidates,
    guesses = allowed_guesses
  )

  n_rounds <- 1
  while(length(state$candidates) > 1) {
    state <- play_round(state, answer, guess)
    i <- 1
    results <- vector("list", length(state$guesses))
    for (guess in state$guesses) {
      results[[i]] <- parLapply(
        cl,
        state$candidates,
        play_game,
        guess = guess,
        candidates = state$candidates) %>%
        bind_rows()
      i <- i + 1
    }

    results <- results %>%
      bind_rows() %>%
      group_by(first_guess) %>%
      summarize(
        mean_rounds = mean(n_rounds)
      )

    guess <- results$first_guess[which.min(results$mean_rounds)]
    n_rounds <- n_rounds + 1
    print(str(state))
    print(n_rounds)
  }

  return(tibble(
    first_guess = first_guess,
    answer = answer,
    n_rounds = n_rounds
  ))
}

answer <- sample(allowed_answers, 1)
guess <- "split"

avg_rand <- replicate(100,
                      play_game(answer, "split", allowed_answers),
                      simplify = FALSE) %>%
  bind_rows()
play_game_optimally(answer, "split", cl, allowed_guesses)




knowledge <- list(
  current_known = c("\\D", "\\D", "\\D", "\\D", "\\D"),
  unknown_location = c(),
  not_in_word = c()
)

# guess 1: split
knowledge[["not_in_word"]] <- c(knowledge$knowledge[["not_in_word"]], "s", "p", "l", "i", "t")

remaining_guesses <- reduce_non_matches(allowed_guesses, knowledge)
remaining_answers <- reduce_non_matches(allowed_answers, knowledge)

cluster <- makeCluster(8)
clusterEvalQ(cluster, library(tidyverse))
clusterExport(cluster,
              c("play_game", "play_round", "update_knowledge",
                "reduce_non_matches"))

# find guess 2
i <- 1
results <- vector("list", length(remaining_guesses))
p <- progress::progress_bar$new(
  total = length(remaining_guesses),
  format = "[:bar] :percent in :elapsed with :eta remaining"
)
for (guess in remaining_guesses) {
  results[[i]] <- parLapply(
    cluster,
    remaining_answers,
    play_game,
    guess = guess,
    candidates = remaining_answers) %>%
    bind_rows()
  i <- i + 1
  p$tick()
}

results %>%
  bind_rows() %>%
  group_by(first_guess) %>%
  summarize(
    expected_rounds = mean(n_rounds)
  ) %>%
  top_n(1, desc(expected_rounds))

# guess 2: cored
knowledge[["not_in_word"]] <- c(knowledge[["not_in_word"]], "c", "o", "r", "d")
knowledge$current_known[[4]] <- "e"

remaining_guesses <- reduce_non_matches(remaining_guesses, knowledge)
remaining_answers <- reduce_non_matches(remaining_answers, knowledge)

# find guess 3
i <- 1
results <- vector("list", length(remaining_guesses))
p <- progress::progress_bar$new(
  total = length(remaining_guesses),
  format = "[:bar] :percent in :elapsed with :eta remaining"
)
for (guess in remaining_guesses) {
  results[[i]] <- parLapply(
    cluster,
    remaining_answers,
    play_game,
    guess = guess,
    candidates = remaining_answers) %>%
    bind_rows()
  i <- i + 1
  p$tick()
}

results %>%
  bind_rows() %>%
  group_by(first_guess) %>%
  summarize(
    expected_rounds = mean(n_rounds)
  ) %>%
  top_n(1, desc(expected_rounds))

# guess 3: axmen
knowledge[["not_in_word"]] <- c(knowledge[["not_in_word"]], "x", "m", "n")
knowledge$current_known[[1]] <- "a"

remaining_guesses <- reduce_non_matches(remaining_guesses, knowledge)
remaining_answers <- reduce_non_matches(remaining_answers, knowledge)
