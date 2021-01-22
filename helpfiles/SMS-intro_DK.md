---
title: 'SMS prognoser'
output:
  html_document:
    keep_md: yes
---


Her kan du læse lidt om baggrunden for denne App. Hvis du ønsker at lave prognoser, skal du trykke på enten **"Simple predictions"** eller **"Detailed predictions"** i toppen af skærmen.... 

### Baggrund
Med denne App kan der laves prognoser for fiskebestandene i Nordsøen ud fra resultaterne fra ICES Stochastic Multispecies Model (SMS). Det fremtidige fiskeritryk og fiskerimønster kan ændres for en række bestande i Nordsøen og modellen beregner derefter det fremtidige fiskeriudbytte og bestandenes størrelse. I beregningerne tages der hensyn til at fisk spiser fisk, så en ændring i fiskeritrykket for en art giver både ændringer i bestandsstørrelse og fiskeriudbytte for arten selv, men også andre arter der kan være byttedyr eller rovdyr for arten.




<!--html_preserve--><table width="70%">
<tr><td colspan=4><b>Modellen har 27 bestande:</td></tr>
<tr><td colspan=4><hr noshade></td></tr>
<tr> <td align=left><b></b></td> <td align=right><b>Bestand</b></td>	<td align=right><b>Engelsk</b></td>	<td align=right><b>Type</b></td> </tr>
<tr> <td align=left><b>1</b></td><td align=right>Mallemuk</td>	<td align=right>Fulmar</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>2</b></td><td align=right>Lomvie</td>	<td align=right>Guillemot</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>3</b></td><td align=right>Sølvmåge</td>	<td align=right>Herring gull</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>4</b></td><td align=right>Ride</td>	<td align=right>Kittiwake</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>5</b></td><td align=right>Svartbag</td>	<td align=right>Great black-backed gull</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>6</b></td><td align=right>Sule</td>	<td align=right>Gannet</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>7</b></td><td align=right>Søpapegøje</td>	<td align=right>Puffin</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>8</b></td><td align=right>Alk</td>	<td align=right>Razorbill</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>9</b></td><td align=right>Tærbe</td>	<td align=right>Starry ray</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>10</b></td><td align=right>Grå knurhane</td>	<td align=right>Grey gurnards</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>11</b></td><td align=right>Vestlig hestemakrel</td>	<td align=right>Western horse mackerel</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>12</b></td><td align=right>Nordsø hestemakrel</td>	<td align=right>North Sea horse mackerel</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>13</b></td><td align=right>Kulmule</td>	<td align=right>Hake</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>14</b></td><td align=right>Gråsæl</td>	<td align=right>Grey seal</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>15</b></td><td align=right>Marsvin</td>	<td align=right>Harbour porpoise</td>	<td align=right>Andet rovdyr</td></tr>
<tr> <td align=left><b>16</b></td><td align=right>Torsk</td>	<td align=right>Cod</td>	<td align=right>Dynamisk, rov- og byttedyr</td></tr>
<tr> <td align=left><b>17</b></td><td align=right>Hvilling</td>	<td align=right>Whiting</td>	<td align=right>Dynamisk, rov- og byttedyr</td></tr>
<tr> <td align=left><b>18</b></td><td align=right>Kuller</td>	<td align=right>Haddock</td>	<td align=right>Dynamisk, rov- og byttedyr</td></tr>
<tr> <td align=left><b>19</b></td><td align=right>Mørksej</td>	<td align=right>Saithe</td>	<td align=right>Dynamisk, rovdyr</td></tr>
<tr> <td align=left><b>20</b></td><td align=right>Makrel</td>	<td align=right>Mackerel</td>	<td align=right>Dynamisk, rovdyr</td></tr>
<tr> <td align=left><b>21</b></td><td align=right>Sild</td>	<td align=right>Herring</td>	<td align=right>Dynamisk byttedyr</td></tr>
<tr> <td align=left><b>22</b></td><td align=right>Tobis, nordlig</td>	<td align=right>Northern sandeel</td>	<td align=right>Dynamisk byttedyr</td></tr>
<tr> <td align=left><b>23</b></td><td align=right>Torbis, sydlig</td>	<td align=right>Southern sandeel</td>	<td align=right>Dynamisk byttedyr</td></tr>
<tr> <td align=left><b>24</b></td><td align=right>Brisling</td>	<td align=right>Sprat</td>	<td align=right>Dynamisk byttedyr</td></tr>
<tr> <td align=left><b>25</b></td><td align=right>Sperling</td>	<td align=right>Norway pout</td>	<td align=right>Dynamisk byttedyr</td></tr>
<tr> <td align=left><b>26</b></td><td align=right>Rødspætte</td>	<td align=right>Plaice</td>	<td align=right>Dynamisk</td></tr>
<tr> <td align=left><b>27</b></td><td align=right>Tunge</td>	<td align=right>Sole</td>	<td align=right>Dynamisk</td></tr>
<tr><td colspan=4><hr noshade></td></tr>
</table><br>
<!--/html_preserve-->
<br>

Gruppen af ”Andet rovdyr” (”Other predators”) indeholder en lang række havfugle, fiskebestande og havpattedyr der spiser fisk. I modellen antages det at man kender antallet af disse rovdyr i prognosen og dette vil holdes konstant i modellen, hvis man da ikke ændre det. Gruppen af ”Dynamiske” bestande indeholder vigtige bestande, hvor bestandstørrelsen udvikles dynamisk ud fra rekruttering, fiskeritryk og den naturlige dødelighed. Nogle af bestandene er både rovdyr og byttedyr, nogle er kun rovdyr eller byttedyr, og nogle er hverken rov- eller byttedyr. 

SMS Modellen anvendes af ICES WGSAM (REF) til at bestemme de historiske naturlige dødeligheder, der anvendes i ICES bestandsvurderingerne og rådgivning for en række bestande i Nordsøen. Modellen er en såkaldt flerartsmodel der ud de historiske fangster, fangstrater fra videnskabelige togter og observeret maveindhold fra en kvart million fisk, samt andre data, beregner de historiske Fiskeridødeligheder (F) og bestandsstørrelser. Modellen indregner at fisk spiser fisk, hvilket resulterer i den såkaldte predationsdødelighed (M2). 

SMS anvendes oftest til at bestemme hvad der er sket historisk, altså til at bestemme de historiske bestandsstørrelser og fiskeridødeligheder. Den historiske SMS giver stort set de samme resultater som i ICES bestandsvurderingen, men der er forskelle, der blandt andet skyldes at SMS er forskellig fra de modeller der oftest benyttes. 

I denne App anvendes SMS som prognosemodel og der regnes frem i tiden. Dette gøres ud fra de model parametre for fx fødepreference og fiskerimønstre, der er bestemt i den historiske SMS samt antagelser om fremtidig fiskeritryk og rekruttering. Prognosen antager, at alt andet end det der ændres i modellen, fx fiskeritrykket, holdes konstant, hvilket er en meget grov antagelse specielt hvis der laves prognoser over mange år. Dette, sammen med en den usikkerhed der altid vil være i en så kompleks model som SMS, betyder at resultatet fra prognoserne skal ses mere som model resultater end som realistiske forudsigelser for hvad der vil ske over en længere årrække.
