
---
title: "Broken Social Contract"
output:
  html_document:
    self_contained: true
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
All over the Western world, we are witnessing an erosion of trust in political institutions and growing anti-establishment sentiment. In the United States, confidence in the three branches of government is at a historic low: Only 27% of Americans trust the Supreme Court, 30% trust the Presidency, and a mere 10% trust Congress.\
\
Why is that? We hypothesize that a core tenet of governance—the social contract between a state and its citizens—is broken. People feel that the state is not living up to its promise, and this informs their political attitudes and behavior.\
\
To test this hypothesis, we conduct three complementary nationally representative studies. In them, we map out the social contract in a data-driven approach (Study 1), we identify the primary broken promises in the social contract (Study 2), and we experimentally manipulate the sentiment of a broken social contract to test its causal impact on political discontent (Study 3). 
</p>

</div>

## Summary

<div style="overflow:auto;">

<img src="assets/specification_curve.png" 
     alt="Linear models of Studies 1 and 2" 
     style="float:left; width:auto; height:auto; max-width:375px; margin:0 0 0.25em 1em;">

<div style="float:left; clear:left; font-size:0.85em; color:#555; font-style:italic; margin:0 0 1em 1em;">
  Linear models of Studies 1 and 2
</div>

<p>
The current research posits that political discontent—the dissatisfaction with, distrust in, and desire to change political institutions—is driven by the subjective experience of a broken social contract. Specifically, we show that those who believe that the government is not living up to its founding promise are more likely to endorse anti-establishment sentiment, support radical change, and distrust national institutions. In Study 1, a nationally representative sample of Americans (N = 1,188) listed the guiding values of the U.S. on paper and the guiding values of the U.S. in practice. The linguistic distance between the two lists in semantic space was positively associated with political discontent. In Study 2, another nationally representative sample of Americans (N = 994), participants rated the government on the eight overarching values of the U.S. on paper that were derived from a computational clustering of Study 1 responses. Again, those who believed the government is not delivering on its founding promise were more likely to display political discontent. In Study 3, another nationally representative sample (N = 1,823) participated in a novel experimental paradigm designed to isolate the causal effect of a broken social contract on political discontent. Those who reflected on the ways in which the U.S. is not delivering on its promise consequently endorsed greater anti-establishment sentiment and radical change. This research highlights the social contract—an implicit agreement between a state and its citizens—as an important lens through which to understand growing political discontent in the U.S.
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

The purpose of Study 1 is to measure Americans' intuitive idea of the social contract, unbiased by researcher framing. Specifically, we asked a nationally representative sample of Americans to list the values that the U.S. stands for on paper (i.e., what was promised), as well as the values that the U.S. stands for in practice (i.e., what is delivered). With word embeddings, participant-assigned weights, and cosine similarity calculus, we are able to measure the subjective experience of a broken social contract: Great distance, in high-dimensional semantic space, between what was promised and what is delivered.\
\
Multilevel linear models, controlling for conservatism, social dominance orientation, agreeableness, gender, race, ethnicity, income, education, age, county median income, county GINI coefficient (i.e., county inequality), and county density, show that a perceived broken social contract **positively predicts anti-establishment sentiment** (&beta; = 0.20, F(19,966) = 6.69, 95% CI [0.14, 0.26], p < .001) **and support for radical change** (&beta; = 0.15, F(19,966) = 4.96, 95% CI [0.09, 0.21], p < .001), and **negatively predicts trust in political democratic institutions** (&beta; = -0.23, F(19,966) = -7.52, 95% CI [-0.29, -0.17], p < .001) **and trust in non-political maintream institutions** (&beta; = -0.20, F(19,966) = -6.61, 95% CI [-0.26, -0.14], p < .001).

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

## Dimension reduction

The written responses, albeit extremely precise in capturing people’s intuitions, posed a problem in detecting the overarching values that people believe guide the U.S. on paper. To detect these overarching values, while staying true to our data-driven approach, we reduced the dimensions of the free-written responses by conducting k-means clustering on the guiding values of the U.S. on paper. The 8-cluster solution that emerged reflect the following overarching values: democracy, equality, freedom, individualism, justice, the pursuit of happiness, the right to bear arms, and tolerance.\
\
Below are is the cluster solution and the five most-mentioned values in each cluster:\
\
<div style="display:flex; flex-wrap:wrap; gap:16px; margin: 0 auto;">

  <!-- Left side: clusters image -->
  <div style="flex:1; min-width:300px; max-width:50%;">
    <img src="assets/clusters.png" alt="K-Means Cluster Solution"
         style="display:block; width:100%; height:auto; margin:0 auto;">
  </div>

  <!-- Right side: table -->
  <div style="flex:1; min-width:300px; max-width:50%; overflow-x:auto;">
    <table style="border-collapse:collapse; width:100%; table-layout:fixed; font-size:0.85em; text-align:center;">
      <tr style="background:#f2f2f2; font-weight:bold;">
        <th style="border:1px solid #ddd; padding:4px; vertical-align:top;">
          <img src="assets/keycluster_1.png" alt="Pursuit of happiness" style="height:40px; width:auto; display:block; margin:0 auto 4px;">
          Pursuit of happiness
        </th>
        <th style="border:1px solid #ddd; padding:4px; vertical-align:top;">
          <img src="assets/keycluster_2.png" alt="Individualism" style="height:40px; width:auto; display:block; margin:0 auto 4px;">
          Individualism
        </th>
        <th style="border:1px solid #ddd; padding:4px; vertical-align:top;">
          <img src="assets/keycluster_3.png" alt="Democracy" style="height:40px; width:auto; display:block; margin:0 auto 4px;">
          Democracy
        </th>
        <th style="border:1px solid #ddd; padding:4px; vertical-align:top;">
          <img src="assets/keycluster_4.png" alt="Equality" style="height:40px; width:auto; display:block; margin:0 auto 4px;">
          Equality
        </th>
        <th style="border:1px solid #ddd; padding:4px; vertical-align:top;">
          <img src="assets/keycluster_5.png" alt="Right to bear arms" style="height:40px; width:auto; display:block; margin:0 auto 4px;">
          Right to bear arms
        </th>
        <th style="border:1px solid #ddd; padding:4px; vertical-align:top;">
          <img src="assets/keycluster_6.png" alt="Freedom" style="height:40px; width:auto; display:block; margin:0 auto 4px;">
          Freedom
        </th>
        <th style="border:1px solid #ddd; padding:4px; vertical-align:top;">
          <img src="assets/keycluster_7.png" alt="Diversity" style="height:40px; width:auto; display:block; margin:0 auto 4px;">
          Diversity
        </th>
        <th style="border:1px solid #ddd; padding:4px; vertical-align:top;">
          <img src="assets/keycluster_8.png" alt="Justice" style="height:40px; width:auto; display:block; margin:0 auto 4px;">
          Justice
        </th>
      </tr>

      <tr>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">opportunity (87)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">independence (143)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">democracy (294)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">equality (398)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">right to bear arms (49)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">freedom (505)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">diversity (59)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">justice (223)</td>
      </tr>

      <tr>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">pursuit of happiness (85)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">individualism (58)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">limited government (23)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">justice for all (20)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">right to vote (37)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">liberty (285)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">fairness (51)</td>
        <td style="border:1px solid #ddd; padding:4px; vertical-align:top;">life (54)</td>
      </tr>

      <!-- continue your rows here ... -->

    </table>
  </div>
</div>

## Study 2

## Study 3





