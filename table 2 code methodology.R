<button class="btn btn-primary" data-toggle="collapse" data-target="#Table2">
  
  Show/Hide data table

</button>
  
  ::: {#Table2 .collapse}
    ```{r table_2_merge, echo = FALSE, results = 'markup'}
    knitr::kable(table_2_merge) %>% kable_styling(font_size = 16,  html_font = 'Arial', bootstrap_options= 'responsive') %>% column_spec(1, bold = TRUE)
    ```
    :::
      
      <strong> Download data for this table </strong>
      
      ::: {style="margin-bottom: 20px;"}
    <a href = download/Table_2.xlsx> <button class="btn pdf"> .xlsx </button> </a><a href = download/Table_2.csv> <button class="btn pdf"> .csv </button></a>
      :::
      
      <br></br>
      