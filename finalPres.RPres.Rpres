Next Word Prediction Application
========================================================
author: Zarmeen Nasim
date: July 2016

Description
========================================================

- Next Word prediction application predicts next word based upon previously typed word or sequence of words.

- It then suggests 5 most likely next words that have higher frequency in the corpus on which the model is trained.

- <a href ="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"> Dataset </a> consisting of tweets,blogs and news is provided by Swiftkey.


How does it works
========================================================

- A sample corpus is created from the dataset provided by taking 20K records from each file(news,tweets and blogs).

- Data is then cleaned using tm package in R

- Unigrams, bigrams, trigrams and quadgrams was build using the DocumentTermMatrix function.

- These Ngrams are then saved in files using saveRDS function for later use.

- Shiny server loads processed ngrams and generates frequency as well as frequency distribution

- Smoothing techniques are applied using Simple Good Turing algorithm and Backoff Algorithm.


How do I use it ?
========================================================

- Visit https://zarmeen.shinyapps.io/finalapp/

- Type word or sentence in the input field and press <Enter>.

- App will then suggest 5 probable next words.

========================================================

<img src = "finalPres.RPres-figure/demo.PNG"/>

Future Enhancement
========================================================

- Use semantic analysis and part of speech to improve predictions

- Implement interpolation technique for smoothing

Thank you
========================================================

Code available at <a href="https://github.com/zarmeen92/datasciencecapstone"> Git Repo </a>
