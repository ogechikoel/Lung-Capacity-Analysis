# 📊 Lung Capacity Analysis in Children and Adolescents

## 📝 Project Overview
This project investigates how **lung capacity (in liters)** varies with demographic and lifestyle factors such as **age**, **sex**, **height**, **smoking status**, and **mode of birth** (cesarean vs. non-cesarean) among children and adolescents. The objective is to identify key predictors of lung capacity using **exploratory data analysis** and **multiple linear regression** in R.

## 📁 Data Source
- Dataset from Kaggle: [Lung Capacity Dataset](https://www.kaggle.com/code/milindmaurya/lung-capacity-dataset)
- Contains **725 observations** and **7 variables**

## 🧰 Tools & Packages Used
- **R version 4.5.0**
- Key R packages:
  - `ggplot2`
  - `dlookr`
  - `flextable`
  - `performance`
  - `car`
  - `broom`

## 📈 Key Findings
- **Height** and **age** are strong positive predictors of lung capacity.
- **Males** have significantly higher lung capacity than females.
- **Smokers** have significantly reduced lung capacity.
- Children born via **cesarean section** show slightly lower lung capacity.
- All key **regression assumptions** were tested and met.

