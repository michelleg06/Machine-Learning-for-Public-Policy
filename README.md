---
title: "EMX+ : An introduction to Machine Learning for Public Policy"
author: "Stephan Dietrich & Michelle González Amador"
#date: '2022-09-21'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(magick)
library(tidyverse)
library(data.table)
library(rmarkdown)
```

```{r pressure, echo=FALSE, fig.cap=" ", out.width = '50%'}
knitr::include_graphics("/Users/michellegonzalez/Desktop/MachineLearning4PP/ML4PP.png")
```
## Introduction

Machine learning has become an increasingly integral part of public policies. It is applied for policy problems that do not require causal inference but instead require predictive inference. Solving these prediction policy problems requires tools that are tuned to minimizing prediction errors, but also frameworks to ensure that models are efficient and fair. EMX+ will introduce the theory and applications of machine learning algorithms with a focus on policy applications and issues. The goals of this course include:

+ Developing a basic understanding of the statistical theory underlying common supervised machine learning algorithms
+ Developing skills necessary to train and assess the performance of selected popular machine learning algorithms for solving public policy problems
+ Gaining an understanding of the benefits and risks of applying machine learning algorithms to public policy problems

The course consists of 5 workshops each consisting of a technical introductory lecture and a hands-on application of the topics to a real-world policy problem. Students will be working with the programming language R, but coding is not the primary focus of the course.

**Course textbook (ebook available for free):**
James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). [An Introduction to Statistical Learning: With Applications in R.](https://hastie.su.domains/ISLR2/ISLRv2_website.pdf)

## 1. Introduction to machine learning for policy analysis

    ·  What is machine learning?
    ·  Inference versus prediction for policy analysis
    ·  Supervised, unsupervised and reinforcement learning
    ·  Regression-classification
    ·  Sampling error vs. specification error
    ·  Bias-variance tradeoff and Mean Square Error (MSE) minimization
    ·  Model validation

**Readings:** 

- An introduction to Statistical learning, Chapter 2.
- Kleinberg, J., Ludwig, J., Mullainathan, S. and Obermeyer, Z., 2015. [Prediction policy problems. American Economic Review, 105(5), pp.491-95.](https://www.aeaweb.org/articles?id=10.1257/aer.p20151023)
- Athey, S. (2017). [Beyond prediction: Using big data for policy problems. Science, 355(6324), 483-485.](https://www.science.org/doi/10.1126/science.aal4321)
- Kleinberg, J., Lakkaraju, H., Leskovec, J., Ludwig, J. and Mullainathan, S., 2017. [Human decisions and machine predictions. The Quarterly Journal of Economics, 133(1), pp.237-293.](https://academic.oup.com/qje/article-abstract/133/1/237/4095198?redirectedFrom=fulltext)

````{r}
#render("EMXMarkdown.Rmd")
```
