## VMO: ShinyApps

The following Shiny Apps were deleveloped for different purposes. Most
of them related to personal (or collaborative) research projects or
teaching.

In order get the apps and deploy them locally you need to execute the
following code on your R console:

    if (!require('shiny')) install.packages("shiny")
    shiny::runGitHub("ShinyApps", "vmoprojs", subdir = "NetCent")

Some applications will be deployed in
[shinyapps.io](https://www.shinyapps.io/) when need. This feature will
be noted in the description if it is available since my subcription only
lets me have 5 apps in the web.

Descriptions of every item are in the next table:

<!-- -   *NetCent*: This App shows the results of the research proyect *A sensitivity measurement of the international trade network in the period 1992-2015* (Una medición de sensibilidad de la red de comercio internacional en el período 1992-2015) developed as a thesis research in [FLACSO](https://www.flacso.edu.ec/) university. [Wilson Pérez-Oviedo](https://www.flacso.edu.ec/portal/docencia/perfil/wilson-perez.1289.1) as well as [John Cajas-Guijarro](https://uce-ec.academia.edu/JohnCajasGuijarro) were research partners in this proyect. -->

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 32%" />
<col style="width: 21%" />
<col style="width: 10%" />
<col style="width: 28%" />
</colgroup>
<thead>
<tr class="header">
<th>App</th>
<th>Project</th>
<th>Collaborators</th>
<th>Institution</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><em>NetCent</em>(<a href="https://vmoprojs.shinyapps.io/NetCent/"
class="uri">https://vmoprojs.shinyapps.io/NetCent/</a>)</td>
<td><em>A sensitivity measurement of the international trade network in
the period 1992-2015</em></td>
<td><a
href="https://www.flacso.edu.ec/portal/docencia/perfil/wilson-perez.1289.1">Wilson
Pérez-Oviedo</a> and <a
href="https://uce-ec.academia.edu/JohnCajasGuijarro">John
Cajas-Guijarro</a></td>
<td><a href="https://www.flacso.edu.ec/">FLACSO</a></td>
<td>Time series (1992-2015) of estimated confidence intervals of
centrality measures (Country-Country Elasticity and PageRank centrality)
through bootstrap are shown. Pairwise Wilcoxon comparisons are also
available.</td>
</tr>
<tr class="even">
<td><em>ClusEvol</em>(<a href="https://vmoprojs.shinyapps.io/ClusEvol/"
class="uri">https://vmoprojs.shinyapps.io/ClusEvol/</a>)</td>
<td><em>Cluster Evolution Analytics: Country macroeconomic
profiles</em></td>
<td><a href="https://orcid.org/0000-0003-4980-8759">Bolívar
Morales-Oñate</a></td>
<td><a href="https://www.uta.edu.ec">UTA</a></td>
<td>We propose Cluster Evolution Analytics (CEA) as a framework that can
be considered in the realm of Advanced Exploratory Data Analysis. CEA
leverages on the temporal component of panel data and it is based on
combining two techniques that are usually not related: leave-one-out and
plug-in principle. This allows us to explore <em>what if</em> questions
in the sense that the present information of an object is plugged-in a
dataset in a previous time frame so that we can explore its evolution
(and of its neighbours) to the present.</td>
</tr>
</tbody>
</table>
