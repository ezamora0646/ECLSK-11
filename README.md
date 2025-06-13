# English Proficiency, Approaches to Learning, and the Moderating Role of Student-Teacher Relationships (SY 2010-2012)

## Table of Contents

- [Scope & Goals of Analysis](#scope-&-goalsof-analysis)
- [Dataset & Overview of Data Structure](#dataset--overview-of-data-structure)
- [Executive Summary](#executive-summary)
- [Insights Deep Dive](#insights-deep-dive)
  - [Comparison of Average Trends & Main Effects](#Comparison-of-Average-Trends-&-Main-Effects)
    - [Demographics: Age & Gender](#demographics-age--gender)
    - [Relationship Scores & Academic Ratings](#Relationship-Scores-&-Academic-Ratings)
- [Recommendations](#recommendations)
  

## Scope & Goals of Analysis
Scientific literature on Multilingual students documents ***disparate achievement outcomes*** for those with lower English proficiency. However, past analyses have not considered external factors such as indicators of the classroom quality. ***Student-teacher relationships have often been overlooked*** when predicting student achievement outcomes, particularly for multilingual students. This analysis aims to explore the degree to which student-teacher relationships ***explains the differences in achievement patterns*** for students with low and high English proficiency.

The **main objectives** of this analysis are to:
1. Replicate findings from 1999 with the most recently available data (2010-2012) to deepen the knowledge base of diverse student populations.
2. Investigate if patterns reported for general multilingual populations exist among Spanish-English multinguals to identify their learning needs.
3. Generate findings that inform culturally and lingusitcally responsive teaching strategies.

## Dataset & Overview of Data Structure

This project focuses on the Spanish-English multilingual student subsample (n= 1200+) of the Early Childhood Longitudinal Study 2011 (ECLS-K:2011) cohort. This is a nationally representative dataset collected by the National Center for Education Statistics <https://nces.ed.gov/ecls/datainformation2011.asp> 

ECLS-K:2011 contains records of over 18,000 students from 1,300 schools across the country. Data collected ranges from direct assessments, teacher surveys, and parent surveys. The outcome of interest is students' Approaches to Learning (ATL) ratings. Variables used in the predictive models demonstrated moderate rates of missing data. Therefore, a multilevel imputation model was fit to address the missing data. Results from EDA were obtained from complete observations, whereas the odds ratios of the estimates reported were obtained with imputed data.

<div align="center">
 <img width="631" alt="Image" src="https://github.com/user-attachments/assets/d09d744d-577e-4b73-a9c3-cbcfe41d2b43" />
</div>


## Executive Summary

In kindergarten (2010-11) female gender (1.45), higher scores in fall ATL (2.80), attentional focus (1.44), as well as closeness (2.77) all increase a student’s odds of receiving a high ATL rating in the spring.

First grade (2011-12) data ***does not indicate*** relationship scores as a moderator of the relationship between English proficiency and ATL ratings. Students with higher conflict were 0.27 times less likely to recieve a high ATL rating. While greater age (1.06), female gender (1.39), and higher attentional focus (1.35) scores were associated with increased likelihood of reciving a high ATL rating.

These findings highlight that students' language status is not a primary predictor of academic achievement. Further analyses should turn to teacher's structure of the classroom and the ways in which it affects students' engagement.

## Insights Deep Dive

### Comparison of Average Trends & Main Effects 

#### Demographics: Age & Gender

#### Girls consistently receive higher ATL ratings — but early estimates may reflect classroom perceptions, not behavior alone. 

<p align="center">
  <img src="https://github.com/user-attachments/assets/bd454431-4622-4466-a37c-4e6baff38dbf" alt="Image 1">
  <img src="https://github.com/user-attachments/assets/193ec398-0c68-4b36-9e3d-2c408caac6b3" alt="Image">
</p>

Depicted is the likelihood of a female student receiving a "High" ATL rating in the kindergarten and first grade years. Across the two years, the likelihood ratios (1.45 & 1.39) indicates a **39%–45% higher likelihood** of receiving a "High" ATL rating, compared to male peers. The wider confidence interval in kindergarten suggests greater variability, possibly influenced by other factors explored below. In contrast, the first grade estimate has a narrower confidence interval, indicating greater precision and suggesting that the observed gender effect is more stable across the first grade student population.


#### Age is a stable but minor predictor of ATL — older students are slightly more likely to be rated

<p align="center">
  <img src="https://github.com/user-attachments/assets/6d9d0593-9089-411b-8b69-a1f459d71995" alt="Image 2">
  <img src="https://github.com/user-attachments/assets/b1d1445d-3c03-4177-bdc7-78153e53ec4f" alt="Image 8">
</p>

Age naturally increases as students progress through grades, so these trends are not depicted. EDA revealed that age was slightly correlated (0.07 - 0.09) with students' ATL scores in both years. This is confirmed by the model, which revealed that older students had a **5-6% higher likelihood** of receiving a ‘High’ ATL rating, a statistically significant but modest effect consistent across years. Again, the estimate and confidence intervals are stable and narrow across the years, indicating that the true population value is within the indicated range.

#### Relationship Scores & Academic Ratings

#### Early student-teacher closeness drives ATL — but the effect fades by first grade.

<p align="center">
  <img src= "https://github.com/user-attachments/assets/21eede1c-1a7e-44ae-8198-11b02f5c2e7e">
  
  <img src="https://github.com/user-attachments/assets/bf919ebc-c4f9-49b4-88c6-67e895b7b6a6">
</p>

Closeness was only measured at the end of the year, when teachers had enough experience with individual students to qualify their relationship so the average trends depict growth over one year. Average trends depict a decrease throughout the year. However, the 1-5 scale of this measure indicates that Latino students on average had close relationships with their teachers across all schools. 

Closeness scores showed a moderate positive correlation (r = .38–.42) with ATL. This relationship is confirmed by the kindergarten model, which reveals that students with a close relationship were **177% more likely** to receive a ‘High’ ATL rating. This relationship **did not hold** in first grade, likely indicating that students' classroom behaviors become more potent predictors of their academic ratings as they progress in school.


#### Conflict undermines ATL across both years — students with conflictual teacher relationships are consistently rated lower.

<p align="center">
  <img src= "https://github.com/user-attachments/assets/2d15f503-fba5-4b90-8906-c15b56e5e64f">
</p>

<p align="center">
  <img src="https://github.com/user-attachments/assets/e3cadcad-1778-458e-9aa8-d77a12b9bebb" alt="Image 4">
  <img src="https://github.com/user-attachments/assets/9e8dfb1b-6c31-4afd-a37c-e8355d38adc8" alt="Image 9">
</p>

Conflict was also measured at the end of the year, so trends depict a one year period. Conflict also decreases overtime but students on average were already at the low end of conflict across schools, as this measure is on a 1-5 scale. 

EDA revealed that conflict with teacher was moderately correlated (-0.47 & -0.61) with students' ATL scores. Both Kindergarten and First grade models confirm this finding, revealing that students experiencing more conflict were **63%–73% less likely** to receive a ‘High’ ATL rating. Confidence intervals for both of the estimates indicate that the true population value would also reflect a decreased likelihood of receiving a "High" ATL rating.

#### Attentional Focus is a key behavioral driver of ATL — and its influence remains strong over time.

<p align="center">
  <img src= "https://github.com/user-attachments/assets/7759bd57-04a8-435b-b3f0-2081ad704842">
</p>

<p align="center">
  <img src="https://github.com/user-attachments/assets/f84d15d0-fd88-4487-b202-5ec2980e29bb" alt="Image 5">
  <img src="https://github.com/user-attachments/assets/6a033b3f-70e5-4580-8bfe-92f410d2160a" alt="Image 10">
</p>
Average trends in students Attention Focus scores appear dynamic with a strong increase throughout kindergarten and a slight decrease after first grade. EDA findings mirror these trends as students' Kindergarten Attentional Focus scores are strongly correlated (0.79) with their ATL scores and moderately correlated (0.49) with their first grade ATL scores. 

The predictive model confirmed that Attentional Focus was the strongest predictor of ATL. Students with higher Attentional Focus scores in the fall of kindergarten were **44% more likely** to receive a "High" ATL rating in the spring and **35% more likely** in First grade. The confidence interval for both years is relatively the same, indicating that true population value is contained within this range. 

#### ATL builds on itself — earlier ratings strongly predict future ratings.

<p align="center">
  <img src="https://github.com/user-attachments/assets/9ecb4bd8-782d-46f4-a476-a840d4bd98c2">
  
  <img src="https://github.com/user-attachments/assets/fd608853-53cb-4aec-beb9-1119818f168e" alt="Image 6">
</p>

Average trends in ATL indicate the students experience more growth in Kindergarten than they do in First grade. EDA reveals a similar story for the correlations between Fall and Spring ATL, with the Fall of Kindergarten scores being more strongly correlated with Spring scores in Kindergarten (0.65) than First grade (0.51). 

Students with higher ATL scores in the fall of kindergarten were **180% more likely** to receive a "High" ATL rating in the spring of that same year. Students with higher Fall ATL ratings in kindergarten were also **50% more likely** to receive a "High" ATL rating in first grade, though this result only approached statistical significance (p = 0.07). This suggests that while early ATL traits have lasting effects, their influence may diminish over time, perhaps explaining why students experience less growth in first grade compared to kindergarten.


## Recommendations
**Strategic takeaway:** Kindergarten sets the tone for how students are rated in later years. Early investments in classroom relationships and behavioral supports can reduce downstream achievement and promote equitable academic trajectories.

**Students with strong teacher relationships in kindergarten are nearly 3x more likely to receive high ATL ratings.**
**Implication:** Building trust early boosts academic confidence and readiness.

**Recommendations** for Program Designers & District Leaders:
1. Invest in relationship-building practices during kindergarten (e.g., smaller class sizes, SEL programs).
2. Emphasize early-year rapport-building in professional development for teachers, given its outsized impact on learning behaviors.

**Students with higher teacher conflict are significantly less likely to receive high ATL ratings — consistently across two years.**
**Implication:** Ongoing conflict may suppress student achievement.

**Recommendations** for District Leaders & Platform Designers:
1. Implement early conflict detection tools (e.g., teacher pulse surveys, behavior tagging).
2. Offer targeted coaching or restorative practices where teacher–student conflict is identified.

**Attentional Focus has the strongest predictive link to ATL across time points.**
**Implication:** Attention skills can signal early academic risk or achievement potential.

**Recommendations** for District Leaders & EdTech Vendors:
1. Use Attentional Focus as a key screener for academic risk in early childhood.
2. Build tools and curricula that target self-regulation and focus (e.g., gamified SEL apps, classroom sensory routines).



