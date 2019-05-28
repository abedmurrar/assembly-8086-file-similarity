# Birzeit University

## Department of Electrical & Computer Engineering

## Second Semester, 2016/201 7

## ENCS 311, Assembly LAB

## Lab Project – Measuring Similarity between Two Sentences

**Objective:**
Write an 8086 assembly program to find the similarity between two sentences.

**Procedure:**

1. Read sentence S1 from S1.txt file and read sentence S2 from S2.txt file.
2. Remove punctuation marks from both sentences.
3. Make all characters as a small letters.
4. Remove Stop words from both sentences. Stop Words are words, which do not contain important
    information. Use the following list of these words:
       [I, a, an, as, at, the, by, in, for, of, on, that]
5. Remove the duplication of words from both sentences. In other words, each word will appear once per
    sentence.
6. Calculate the similarity as the size of the intersection of words between the two processed sentences
    divided by the size of the union of the two processed sentences:

<img src="https://latex.codecogs.com/png.latex?\dpi{120}&space;\bg_white&space;\large&space;Similarity=\frac{(S_1&space;\cap&space;S_2)}{(S_1&space;\cup&space;s_2)}" title="\large Similarity=\frac{(S_1 \cap S_2)}{(S_1 \cup s_2)}" />

```
A value “0” means the two sentences are completely dissimilar, “1” that they are identical, and values
between 0 and 1 representing a degree of similarity.
```
