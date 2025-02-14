# General Pharaoh Index of Runes  𓋖

## 1. Introduction

The **General Pharaoh Index of Runes (GPIR)** is an innovative and automated index designed to identify and track the **most important runes** in the Bitcoin ecosystem. Inspired by the grandeur and legacy of the ancient Egyptian pharaohs, this index combines blockchain technology, real-time data, and historical symbolism to provide a reliable and transparent metric for evaluating the value, adoption, and activity of runes.

### The Power of the Pharaoh: Automation and Objectivity
In ancient Egypt, the pharaoh was a divine leader whose decisions were guided by higher principles. In the **General Pharaoh Index of Runes**, the "power of the pharaoh" translates into a **100% automated system** that selects and weights runes objectively, without human intervention. Runes are not chosen by people but by algorithms analyzing real-time data, ensuring fairness and transparency.

### The Legacy of Egyptian Crowns 
### 𓋔 𓋗 𓋓 𓋖 𓋕 𓋘 𓋒 𓋛 𓋑 𓋙 𓋚

In the BRC-20 protocol of Bitcoin, it is possible to inscribe 4-byte symbols, including Egyptian hieroglyphs. The **GPIR** is the **proud and exclusive owner of all inscribed Egyptian crowns**, a collection of unique and historically significant symbols that now live on the Bitcoin blockchain. These crowns are not just digital assets; they are a bridge between ancient history and modern technology.

The Egyptian crowns—such as the **𓋑 Hedjet** (White Crown of Upper Egypt) and the **𓋔 Deshret** (Red Crown of Lower Egypt) are more than mere symbols. They represent **power, authority, and unity**, values that are deeply embedded in the **GPIR**. By owning these crowns, the index not only pays homage to the cultural richness of ancient Egypt but also establishes itself as a **guardian of history and a pioneer in the Bitcoin ecosystem**.

This unique ownership reinforces the **authority and exclusivity** of the **General Pharaoh Index of Runes**, setting it apart as a project that combines **historical legacy** with **cutting-edge blockchain technology**. The crowns are a testament to the strength and resilience of Bitcoin, just as they were symbols of strength and resilience in ancient times.

### A Project with a Vision for the Future

The **General Pharaoh Index of Runes** is not just a technical tool; it is a **tribute to history and culture**, represented by the Egyptian crowns inscribed on Bitcoin. And, like the pharaoh, its power lies in objectivity and automation. This project aspires to become a reliable standard for investors, developers, and enthusiasts in the rune ecosystem


## 2.Methodology

The **General Pharaoh Index of Runes (GPIR)** Index uses a fully automated and data-driven approach to select and weigh the most important runes in the Bitcoin ecosystem. The methodology is designed to ensure transparency, fairness, and accuracy, eliminating any human bias. Below, we break down the key components of the selection process.

### 2.1. Data Collection

The index relies on real-time data to evaluate runes based on market capitalization, number of holders, and transaction volume. Currently, the primary data source is the Unisat API, a reliable provider of Bitcoin rune data. However, we acknowledge that Unisat is not the most liquid platform, and we are actively working to integrate the Geniidata API soon to enhance data accuracy and coverage.

### 2.2 Variables Used in the Selection of Runes for the Pharaoh Index

The **General Pharaoh Index of Runes** evaluates runes based on three primary variables:

#### 2.2.1. Market Capitalization (MC)
- **What it measures:** The total market value of a rune.
- **Why it matters:** Reflects the size and economic significance of a rune.

#### 2.2.2. Number of Holders
- **What it measures:** The total number of unique addresses holding a rune.
- **Why it matters:** Indicates broader distribution and adoption.

#### 2.2.3. Number of Transactions
- **What it measures:** The total number of transactions involving a rune over a specific period.
- **Why it matters:** Reflects active use and community engagement.

#### 2.2.4. Number of Mentions in Social (Future input)
- **What it measures:** The total number of mentions of the token on social media over a specified period of time.
- **Why it matters:** Why it matters: Reflects the level of community engagement and activity.

### 2.3 Optimal Number of Clusters (Automated Elbow Method)

To determine the optimal number of clusters automatically, we implemented an **Automated Elbow Method**:

**WCSS Calculation**: K-Means was applied for \( k = 1 \) to \( k = 10 \), and WCSS (Within-Cluster Sum of Squares) was computed.

**Second Derivative**: The second derivative of the WCSS curve was calculated to identify the point of maximum curvature (the "elbow").

**Optimal \( k \) Selection**: The algorithm automatically selected the \( k \) at the elbow point as the optimal number of clusters.

This approach eliminates visual subjectivity and ensures reproducibility.


### 2.4 Clustering

After determining the optimal number of clusters (\( k \)), we applied the **K-Means** algorithm to group the data:

**Centroid Initialization**: Centroids were initialized using the K-Means++ method.

**Point Assignment**: Each data point was assigned to the nearest centroid using Euclidean distance.

**Centroid Update**: Centroids were recalculated as the mean of all points in the cluster.

**Iteration**: The process was repeated until convergence.


### 2.5 Selection of Clusters

After clustering, the most relevant clusters were selected based on:

1. **Cluster Profiling**: Size, centroid values, and distribution.
2. **Quality Metrics**: Silhouette Score and Inertia (WCSS).
3. **Relevance to Objectives**: Alignment with the goals of the Pharaoh Index.
4. **Outlier Detection**: Handling of small or sparse clusters.

The selected clusters were used as inputs for the Pharaoh Index calculation.

### 2.6 Elaboration of the General Pharaoh Index of Runes (GPIR)

