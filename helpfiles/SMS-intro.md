---
title: 'SMS prognoser'
output:
  html_document:
    keep_md: yes
---

This section provides some background information for this App. To make forecasts, you have to push **"Simple predictions"** or **"Detailed predictions"** in the top of the screen.The calculations for the Simple predictions are the same as for the Detailed predictions, but simpler output are produced for the simple prediction, so this model type runs faster.


### Background
This App makes forecast scenarios for the main fish stocks in the North Sea, based on the results from the Stochastic Multispecies Model (SMS) used by ICES to provide multispecies mortalities. Future fishing pressure and exploitation pattern can be changed for the stocks and the model will then calculate future fishing yield and stock sizes. SMS takes into account that fish eats fish, so a change in fishing pressure for a given species will directly change its stock size and yield, but may also change stock size and yield for other species which are prey or predator for the given species.   




<!--html_preserve--><table width="70%">
<tr><td colspan=4><b>The model has 27 stocks:</td></tr>
<tr><td colspan=4><hr noshade></td></tr>
<tr> <td align=left><b></b></td> <td align=right><b>Danish stock name</b></td>	<td align=right><b>Stock name</b></td>	<td align=right><b>Type</b></td> </tr>
<tr> <td align=left><b>1</b></td><td align=right>Mallemuk</td>	<td align=right>Fulmar</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>2</b></td><td align=right>Lomvie</td>	<td align=right>Guillemot</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>3</b></td><td align=right>Sølvmåge</td>	<td align=right>Herring gull</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>4</b></td><td align=right>Ride</td>	<td align=right>Kittiwake</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>5</b></td><td align=right>Svartbag</td>	<td align=right>Great black-backed gull</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>6</b></td><td align=right>Sule</td>	<td align=right>Gannet</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>7</b></td><td align=right>Søpapegøje</td>	<td align=right>Puffin</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>8</b></td><td align=right>Alk</td>	<td align=right>Razorbill</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>9</b></td><td align=right>Tærbe</td>	<td align=right>Starry ray</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>10</b></td><td align=right>Grå knurhane</td>	<td align=right>Grey gurnards</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>11</b></td><td align=right>Vestlig hestemakrel</td>	<td align=right>Western horse mackerel</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>12</b></td><td align=right>Nordsø hestemakrel</td>	<td align=right>North Sea horse mackerel</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>13</b></td><td align=right>Kulmule</td>	<td align=right>Hake</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>14</b></td><td align=right>Gråsæl</td>	<td align=right>Grey seal</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>15</b></td><td align=right>Marsvin</td>	<td align=right>Harbour porpoise</td>	<td align=right>Other predator</td></tr>
<tr> <td align=left><b>16</b></td><td align=right>Torsk</td>	<td align=right>Cod</td>	<td align=right>Dynamic predator and prey</td></tr>
<tr> <td align=left><b>17</b></td><td align=right>Hvilling</td>	<td align=right>Whiting</td>	<td align=right>Dynamic predator and prey</td></tr>
<tr> <td align=left><b>18</b></td><td align=right>Kuller</td>	<td align=right>Haddock</td>	<td align=right>Dynamic predator and prey</td></tr>
<tr> <td align=left><b>19</b></td><td align=right>Mørksej</td>	<td align=right>Saithe</td>	<td align=right>Dynamic predator</td></tr>
<tr> <td align=left><b>20</b></td><td align=right>Makrel</td>	<td align=right>Mackerel</td>	<td align=right>Dynamic predator</td></tr>
<tr> <td align=left><b>21</b></td><td align=right>Sild</td>	<td align=right>Herring</td>	<td align=right>Dynamic prey</td></tr>
<tr> <td align=left><b>22</b></td><td align=right>Tobis, nordlig</td>	<td align=right>Northern sandeel</td>	<td align=right>Dynamic prey</td></tr>
<tr> <td align=left><b>23</b></td><td align=right>Torbis, sydlig</td>	<td align=right>Southern sandeel</td>	<td align=right>Dynamic prey</td></tr>
<tr> <td align=left><b>24</b></td><td align=right>Brisling</td>	<td align=right>Sprat</td>	<td align=right>Dynamic prey</td></tr>
<tr> <td align=left><b>25</b></td><td align=right>Sperling</td>	<td align=right>Norway pout</td>	<td align=right>Dynamic prey</td></tr>
<tr> <td align=left><b>26</b></td><td align=right>Rødspætte</td>	<td align=right>Plaice</td>	<td align=right>Dynamic</td></tr>
<tr> <td align=left><b>27</b></td><td align=right>Tunge</td>	<td align=right>Sole</td>	<td align=right>Dynamic</td></tr>
<tr><td colspan=4><hr noshade></td></tr>
</table><br>
<!--/html_preserve-->
<br>

There are two main types of stocks shown in the table: "Other predator" and "Dynamic stocks". The group of **Other predators** includes a number of sea  birds, fish stocks and marine mammals, which all eat fish. It is assumed that the we know the abundance of these predators in the model, which will be be kept constant if the user do not change it. The group of **Dynamic stocks** includes the main fish stocks, where stock sizes change dynamically from recruitment, fishing pressure and natural mortality. Some of the stocks act both as predator and prey species, while some are only predators or preys in the model. Plaice and sole do not eat any of the fish stocks in the model (at a significant level) or is prey (at a significant level) for the model predators.

The SAM model is used by the ICES Working Group on Multispecies Assessment Method, WGSAM (ICES, 2021) to estimate the historical natural mortalities, which are used in the ICES stock assessment and TAC advice for number of species in the North Sea area. The model is a so-called multispecies model, which uses historical catches. catch rates from scientific surveys and the observed stomach content from a quarter of a million fish to estimate the historical fishing mortality (F) and stock sizes. The model takes account for predation, which results in the so-called predation mortality (M2). 

SMS is most often applied to estimate the historical stock sizes and fishing mortalities. The results are close to the results from the ICES single stock assessments, but not identical as the the SMS model as the ICES models for the individual species are often more complex than SMS. 

In this App, SMS is used as a forecast model. This is done from the model parameters, e.g. food suitability and exploitation pattern, estimated in the historical SMS, and assumptions of future fishing pressure and recruitment. The forecasts or scenarios assumes that everything is kept constant in the future, if not changed by the user. The is a crude assumption when the forecast is made for a long time period. This and the fact that there in general is rather high uncertainties in a complex model like SMS, means that the results should be seen a model results rather than strict prediction of future changes.


