library(qiverse)
library(data.table)

tk <- get_az_tk('pbi_df')

wa_pci_operators <- download_dataflow_table(
  workspace_name = 'PCI Data Set',
  dataflow_name = 'ref_pci_operators',
  table_name = 'wa_pci_operators',
  access_token = tk$credentials$access_token
) |>
  as.data.table()

