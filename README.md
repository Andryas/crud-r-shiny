
The purpose of this app its to provide a better way to create editable in r shiny,
instead using DT or rhansontable, create your own module that fits your needs.

---

Have you ever come across the term CRUD? It stands for
Create-Read-Update-Delete. I recently discovered it, and it's a fundamental
concept in applications where you inevitably find yourself creating new
entities, reading their information, updating as necessary, and, of course,
deleting when they're no longer needed.

While in Shiny, the need for CRUD operations isn't as common because the typical
flow of an application involves connecting to databases, processing data,
reshaping it, and presenting visualizations with options for interaction, such
as filtering. However, there are rare cases where you need to gather inputs from
users, display them row by row, and simultaneously use the dataset created to
generate results.

In my recent project, we developed a feasibility model in Shiny to analyze the
financial viability of a real estate project. This involved evaluating various
factors like construction costs, sales projections, land acquisition expenses,
and more. In one part of the application, we needed to collect information about
the types of units to be built in a particular development. This is where the
need for a more efficient form-filling process arose.

Essentially, we needed to gather the following information for each unit type to
progress in our model:

- name: What is this common name referred to as? 
- area: What is the total private area of each unit? 
- count: How many units of this type will be built?
- price_per_sqm: What is the anticipated selling price per square meter, as
  indicated by the developer? 
- exchange: How many units of this type will be allocated for land payment? 
- unit: What is the intended purpose of each unit? Residential? Commercial?
 
Improving the form-filling process for collecting this information was crucial
for streamlining our analysis and decision-making process within the feasibility
model.

I understand  that there are various methods for directly editing tables using
packages like DT or rhandsontable. However, a significant drawback of these
approaches is their limitation in validating user inputs and their lack of
support for sophisticated input gathering methods. They don't allow for the
utilization of functions from shinyWidget or even shiny, which offer more
advanced capabilities.

Without further ado, the application.

![](https://storage.googleapis.com/varvenza/05-a-different-approach-to-editing-tables-in-r-shiny/Screenshot%202024-03-08%20at%205.28.38%E2%80%AFPM.png)

As you can see in the top-left, we have a "+" button which, when clicked, opens
a modal.

![](https://storage.googleapis.com/varvenza/05-a-different-approach-to-editing-tables-in-r-shiny/Screenshot%202024-03-12%20at%203.28.34%E2%80%AFPM.png)

And if we fill the forms wrong ...

![](https://storage.googleapis.com/varvenza/05-a-different-approach-to-editing-tables-in-r-shiny/Screenshot%202024-03-12%20at%203.45.45%E2%80%AFPM.png)

And if you fill right ...

![](https://storage.googleapis.com/varvenza/05-a-different-approach-to-editing-tables-in-r-shiny/Screenshot%202024-03-12%20at%203.47.19%E2%80%AFPM.png)

As you can see in the right side we have two buttons, one for editing (update) and
one for delete. 

If you click on the edit button, you are going to see this

![](https://storage.googleapis.com/varvenza/05-a-different-approach-to-editing-tables-in-r-shiny/Screenshot%202024-03-12%20at%203.48.22%E2%80%AFPM.png)

It works exactly like the adding button, but in this case, we are changing the
forms we created previously.

And if you delete...

![](https://storage.googleapis.com/varvenza/05-a-different-approach-to-editing-tables-in-r-shiny/Screenshot%202024-03-08%20at%205.28.38%E2%80%AFPM.png)

We comeback to the starting point.

Hope you find it useful

Cheers

