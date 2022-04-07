# NOVA SBE Publications Network Analytics

*Final group project for the course Network Analytics. We are analyzing the network of research article collaborations at Nova School of Business & Economics and authors from related institutions.*

![alt text](https://github.com/fynnoldenburg/NOVA-SBE-Publications-Network-Analytics/blob/main/Total_Network_Plot.png?raw=true)

## Methodology Description:

We get the raw data by scraping the "Articles" accordion element on the website https://www.novasbe.unl.pt/en/faculty-research/research/publications. Consequently, we extract relevant attributes authors, title and year from the citations through string manipulation. We additionally scrape a list of NOVA authors which are highlighted by a "strong" html tag on the website and append this as a feature to the final data frame.

We are interested in the relationships of authors. Therefore, we create a bipartite projection in the authors space of our data. This graph gives us all authors as vertices while the edge weights between them indicate the number of articles the two authors have collaborated on.

After experimenting with several configuartions we decided to display a network of authors with at least 10 collaborations to another author (gold). We also include external authors (grey) to give an indication on NOVA's external research network and the most active authors in that regard.
