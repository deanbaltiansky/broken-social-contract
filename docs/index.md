---
title: "Broken Social Contract"
output:
  html_document:
    self_contained: true
    md_extensions: +raw_html
---

# Political Discontent is Driven by A Broken Social Contract

<img src="assets/banner-flag.png" alt="Flag Banner" style="width:100%; height:auto;">

## Background

<div style="overflow:auto;">

<img src="assets/trust_in_institutions.png" 
     alt="Declining trust in institutions (Gallup)" 
     style="float:right; width:auto; height:auto; max-width:375px; margin:0 0 0.25em 1em;">

<div style="float:right; clear:right; font-size:0.85em; color:#555; font-style:italic; margin:0 0 1em 1em;">
  Source: Gallup
</div>

<p>
All over the Western world, we are witnessing an erosion of trust in political institutions and growing anti-establishment sentiment. In the United States, confidence in the three branches of government is at a historic low: Only 27% of Americans trust the Supreme Court, 30% trust the Presidency, and a mere 10% trust Congress.
</p>
<p>
Why is that? We hypothesize that a core tenet of governance—the social contract between a state and its citizens—is broken. People feel that the state is not living up to its promise, and this informs their political attitudes and behavior.
</p>
<p>
To test this hypothesis, we conduct three complementary nationally representative studies. In them, we map out the social contract in a data-driven approach (Study 1), we identify the primary broken promises in the social contract (Study 2), and we experimentally manipulate the sentiment of a broken social contract to test its causal impact on political discontent (Study 3).
</p>

</div>

## Summary

<div style="overflow:auto;">

<p>
The current research posits that political discontent—the dissatisfaction with, distrust in, and desire to change political institutions—is driven by the subjective experience of a broken social contract. Specifically, we show that those who believe the government is not living up to its founding promise are more likely to endorse anti-establishment sentiment, support radical change, and distrust national institutions. In Study 1, a nationally representative sample of Americans (N = 1,188) listed the guiding values of the U.S. on paper and the guiding values of the U.S. in practice. The linguistic distance between the two lists in semantic space was positively associated with political discontent. In Study 2 (N = 994), participants rated the government on the eight overarching values of the U.S. on paper that were derived from a computational clustering of Study 1 responses. Again, those who believed the government is not delivering on its founding promise were more likely to display political discontent. In Study 3 (N = 1,823), a novel experimental paradigm isolated the causal effect of a broken social contract on political discontent: prompting participants to reflect on the U.S. not delivering on its promise increased anti-establishment sentiment and support for radical change.
</p>

</div>

## Study 1

<div style="margin-top: 0;">
  <p style="margin:0 0 12px 0;">
    <a href="./studies/study-1/index.html" target="_blank" rel="noopener"
       style="background:#0b69ff;color:white;padding:0.55em 0.9em;border-radius:8px;text-decoration:none;display:inline-block;margin:0;">
      FULL REPORT ↗
    </a>
  </p>
</div>

<p>
The purpose of Study 1 is to measure Americans' intuitive idea of the social contract, unbiased by researcher framing. Specifically, we asked a nationally representative sample of Americans to list the values that the U.S. stands for on paper (i.e., what was promised), as well as the values that the U.S. stands for in practice (i.e., what is delivered). With word embeddings, participant-assigned weights, and cosine similarity calculus, we measure the subjective experience of a broken social contract: great distance, in high-dimensional semantic space, between what was promised and what is delivered.
</p>
<p>
Multilevel linear models, controlling for conservatism, social dominance orientation, agreeableness, gender, race, ethnicity, income, education, age, county median income, county GINI coefficient (i.e., county inequality), and county density, show that a perceived broken social contract <strong>positively predicts anti-establishment sentiment</strong> (β = 0.21, F(19,993) = 6.82, 95% CI [0.15, 0.26], p < .001) <strong>and support for radical change</strong> (β = 0.15, F(19,993) = 5.24, 95% CI [0.10, 0.21], p < .001), and <strong>negatively predicts trust in political democratic institutions</strong> (β = -0.23, F(19,993) = -7.58, 95% CI [-0.29, -0.17], p < .001) and <strong>trust in non-political mainstream institutions</strong> (β = -0.20, F(19,993) = -6.63, 95% CI [-0.25, -0.14], p < .001).
</p>

<div style="margin-top: 0;">
  <p style="margin:0 0 12px 0;">
    <a href="./studies/study-1/app/index.html" target="_blank" rel="noopener"
       style="background:#0b69ff;color:white;padding:0.55em 0.9em;border-radius:8px;text-decoration:none;display:inline-block;margin:0;">
      EXPLORE CORRELATIONS ↗
    </a>
  </p>

  <p style="margin:0;">
    <a href="./studies/study-1/lm-table-app/index.html" target="_blank" rel="noopener"
       style="background:#0b69ff;color:white;padding:0.55em 0.9em;border-radius:8px;text-decoration:none;display:inline-block;margin:0;">
      EXPLORE OLS MODELS ↗
    </a>
  </p>
</div>

```{=html}
<div style="display:flex; align-items:center; gap:20px; margin:16px 0 24px;">
  <div style="flex:0 0 70%;">
    <img src="assets/specification_curve.png" 
         alt="Linear models of Studies 1 and 2"
         style="width:100%; height:auto; display:block; margin:0;">
  </div>

  <div style="flex:1; font-size:0.9em; color:#333;">
    <p><strong>Specification Curve: Linear models of Studies 1 and 2.</strong> The figure shows the standardized beta coefficients of a broken social contract on antiestablishment sentiment, support for radical change, trust in democratic instituions, trust in mainstream national non-political institutions, and intent to vote in Nov. 2024 (data collected in March 2024 and May 2024). The specification curve demonstrates the robustness of the effect, controlling for a wide variety of covariates, all of which have been shown in the past to predict important political attitudes outcomes. Error bars represent 95% Confidence Intervals.</p>
  </div>
</div>
```

## Dimension reduction

<p>
The written responses, albeit precise in capturing people’s intuitions, posed a problem in detecting the overarching values that people believe guide the U.S. on paper. To detect these overarching values while staying true to our data-driven approach, we reduced the dimensions of the free-written responses by conducting k-means clustering on the guiding values of the U.S. on paper. The 8-cluster solution that emerged reflects the following overarching values: democracy, equality, freedom, individualism, justice, the pursuit of happiness, the right to bear arms, and tolerance.
</p>

<p>Below is the cluster solution and the five most-mentioned values in each cluster:</p>

<table width="100%" style="border-collapse:collapse; table-layout:fixed;">
  <tr style="vertical-align:top;">
    <!-- Left column: image -->
    <td style="width:35%; padding:0 8px 0 0; vertical-align:top;">
      <img src="assets/clusters.png" alt="K-Means Cluster Solution" style="display:block; width:100%; height:auto;">
    </td>

    <!-- Right column: table -->
    <td style="width:65%; padding:0 0 0 8px; vertical-align:top;">
      <table style="border-collapse:collapse; width:100%; font-size:0.75em; text-align:left; word-wrap:break-word; overflow-wrap:break-word;">
        <tr style="background:#f2f2f2; font-weight:bold;">
          <th style="border:1px solid #ddd; padding:3px; vertical-align:top; text-align:left;">
            <img src="assets/keycluster_1.png" alt="Pursuit of happiness" style="height:36px; display:block; margin:0 auto 4px;">
            Pursuit of happiness
          </th>
          <th style="border:1px solid #ddd; padding:3px; vertical-align:top; text-align:left;">
            <img src="assets/keycluster_2.png" alt="Individualism" style="height:36px; display:block; margin:0 auto 4px;">
            Individualism
          </th>
          <th style="border:1px solid #ddd; padding:3px; vertical-align:top; text-align:left;">
            <img src="assets/keycluster_3.png" alt="Democracy" style="height:36px; display:block; margin:0 auto 4px;">
            Democracy
          </th>
          <th style="border:1px solid #ddd; padding:3px; vertical-align:top; text-align:left;">
            <img src="assets/keycluster_4.png" alt="Equality" style="height:36px; display:block; margin:0 auto 4px;">
            Equality
          </th>
          <th style="border:1px solid #ddd; padding:3px; vertical-align:top; text-align:left;">
            <img src="assets/keycluster_5.png" alt="Right to bear arms" style="height:36px; display:block; margin:0 auto 4px;">
            Right to bear arms
          </th>
          <th style="border:1px solid #ddd; padding:3px; vertical-align:top; text-align:left;">
            <img src="assets/keycluster_6.png" alt="Freedom" style="height:36px; display:block; margin:0 auto 4px;">
            Freedom
          </th>
          <th style="border:1px solid #ddd; padding:3px; vertical-align:top; text-align:left;">
            <img src="assets/keycluster_7.png" alt="Diversity" style="height:36px; display:block; margin:0 auto 4px;">
            Diversity
          </th>
          <th style="border:1px solid #ddd; padding:3px; vertical-align:top; text-align:left;">
            <img src="assets/keycluster_8.png" alt="Justice" style="height:36px; display:block; margin:0 auto 4px;">
            Justice
          </th>
        </tr>

        <tr>
          <td style="border:1px solid #ddd; padding:3px;">opportunity (87)</td>
          <td style="border:1px solid #ddd; padding:3px;">independence (143)</td>
          <td style="border:1px solid #ddd; padding:3px;">democracy (294)</td>
          <td style="border:1px solid #ddd; padding:3px;">equality (398)</td>
          <td style="border:1px solid #ddd; padding:3px;">right to bear arms (49)</td>
          <td style="border:1px solid #ddd; padding:3px;">freedom (505)</td>
          <td style="border:1px solid #ddd; padding:3px;">diversity (59)</td>
          <td style="border:1px solid #ddd; padding:3px;">justice (223)</td>
        </tr>

        <tr>
          <td style="border:1px solid #ddd; padding:3px;">pursuit of happiness (85)</td>
          <td style="border:1px solid #ddd; padding:3px;">individualism (58)</td>
          <td style="border:1px solid #ddd; padding:3px;">limited government (23)</td>
          <td style="border:1px solid #ddd; padding:3px;">justice for all (20)</td>
          <td style="border:1px solid #ddd; padding:3px;">right to vote (37)</td>
          <td style="border:1px solid #ddd; padding:3px;">liberty (285)</td>
          <td style="border:1px solid #ddd; padding:3px;">fairness (51)</td>
          <td style="border:1px solid #ddd; padding:3px;">life (54)</td>
        </tr>

        <tr>
          <td style="border:1px solid #ddd; padding:3px;">happiness (33)</td>
          <td style="border:1px solid #ddd; padding:3px;">individuality (15)</td>
          <td style="border:1px solid #ddd; padding:3px;">rule of law (21)</td>
          <td style="border:1px solid #ddd; padding:3px;">equal rights (16)</td>
          <td style="border:1px solid #ddd; padding:3px;">rights (28)</td>
          <td style="border:1px solid #ddd; padding:3px;">freedom of speech (196)</td>
          <td style="border:1px solid #ddd; padding:3px;">religion (25)</td>
          <td style="border:1px solid #ddd; padding:3px;">unity (45)</td>
        </tr>

        <tr>
          <td style="border:1px solid #ddd; padding:3px;">capitalism (31)</td>
          <td style="border:1px solid #ddd; padding:3px;">self-determination (10)</td>
          <td style="border:1px solid #ddd; padding:3px;">checks and balances (13)</td>
          <td style="border:1px solid #ddd; padding:3px;">equality for all (16)</td>
          <td style="border:1px solid #ddd; padding:3px;">individual rights (24)</td>
          <td style="border:1px solid #ddd; padding:3px;">freedom of religion (137)</td>
          <td style="border:1px solid #ddd; padding:3px;">honesty (22)</td>
          <td style="border:1px solid #ddd; padding:3px;">peace (28)</td>
        </tr>

        <tr>
          <td style="border:1px solid #ddd; padding:3px;">hard work (28)</td>
          <td style="border:1px solid #ddd; padding:3px;">sovereignty (8)</td>
          <td style="border:1px solid #ddd; padding:3px;">separation of powers (9)</td>
          <td style="border:1px solid #ddd; padding:3px;">equal opportunity (9)</td>
          <td style="border:1px solid #ddd; padding:3px;">human rights (17)</td>
          <td style="border:1px solid #ddd; padding:3px;">free speech (55)</td>
          <td style="border:1px solid #ddd; padding:3px;">integrity (21)</td>
          <td style="border:1px solid #ddd; padding:3px;">progress (24)</td>
        </tr>
      </table>
    </td>
  </tr>
</table>



## Study 2

<div style="margin-top: 0;">
  <p style="margin:0 0 12px 0;">
    <a href="./studies/study-2/index.html" target="_blank" rel="noopener"
       style="background:#0b69ff;color:white;padding:0.55em 0.9em;border-radius:8px;text-decoration:none;display:inline-block;margin:0;">
      FULL REPORT ↗
    </a>
  </p>
</div>

<p>
The purpose of Study 2 is two-fold: (1) replicate the effects of a broken social contract on political discontent, as observed in Study 1; (2) identify the overarching values that drive this effect. In other words, Study 2 helps us uncover which values are perceived to be under-delivered by the state, which ones are the most important predictors of political discontent, and whether some people care more about some values and others care about other values.
</p>
<p>
To that end, we showed participants the wight overarching values that resulted from the dimension reduction process of free-written responses in Study 1: (1) Democracy; (2) Equality; (3) Freedom; (4) Individualism; (5) Justice; (6) Pursuit of Happiness; (7) Right to Bear Arms; and (8) Tolerance. With a forced sum (must total 100%) they were asked to indicate their perception of priorities of the U.S. on paper as they relate to these values. Then, they indicated the extent to which they believed the U.S. lives up to each of these values (0-100 score).
</p>
<p>
The broken promise score is a weighted mean of the “values delivered” score, weighted by the perceived priorities of the U.S., as indicated by the participant. To get to this weighted mean, each score assigned to the values delivered by the U.S. government was weighted by the participant-assigned priorities indicated in the “priorities of the U.S. on paper” measure. That is, we multiplied the score (0-100) of each value by the weight of the value and took the sum of all weighted value scores. Then, we reverse-scored that sum by subtracting it from 100 so that higher scores indicate a more broken promise.
</p>

<div style="margin-top: 0;">
  <p style="margin:0 0 12px 0;">
    <a href="./studies/study-2/app/index.html" target="_blank" rel="noopener"
       style="background:#0b69ff;color:white;padding:0.55em 0.9em;border-radius:8px;text-decoration:none;display:inline-block;margin:0;">
      EXPLORE CORRELATIONS ↗
    </a>
  </p>

  <p style="margin:0;">
    <a href="./studies/study-2/lm-table-app/index.html" target="_blank" rel="noopener"
       style="background:#0b69ff;color:white;padding:0.55em 0.9em;border-radius:8px;text-decoration:none;display:inline-block;margin:0;">
      EXPLORE OLS MODELS ↗
    </a>
  </p>
</div>

<p>
Multilevel linear models, controlling for conservatism, social dominance orientation, agreeableness, gender, race, ethnicity, income, education, age, county median income, county GINI coefficient (i.e., county inequality), and county density, show that a perceived broken social promise <strong>positively predicts anti-establishment sentiment</strong> (β = 0.34, F(19,794) = 10.18, 95% CI [0.27, 0.40], p < .001) <strong>and support for radical change</strong> (β = 0.30, F(19,794) = 8.84, 95% CI [0.24, 0.37], p < .001), and <strong>negatively predicts trust in political democratic institutions</strong> (β = -0.38, F(19,794) = -11.69, 95% CI [-0.44, -0.31], p < .001) and <strong>trust in non-political mainstream institutions</strong> (β = -0.37, F(19,794) = -11.53, 95% CI [-0.43, -0.30], p < .001).
</p>
## Study 3