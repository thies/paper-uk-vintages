Merging Data
====

Sales
---
Use preprocessd Land Reistry sales data: [./sales/sales_cambridge.csv](sales/sales_cambridge.csv)

 Addresses for Geocoding
--
**THESE DATA ARE CONFIDENTIAL**
The [AddressBase](https://www.ordnancesurvey.co.uk/business-and-government/products/addressbase-products.html) data translates address strings to e.g. TOIDs. These TOIDs can be linked to the shapes of properties in the OS maps.

Ordnance Survey Maps
---

Derived data sets
---
Derived data are stored in folder: [derived](./derived)

Merge scripts
---

- **merge_data_sales_addresses.R**
(this really needs to be improved, we lose about 2/3 of the sales data right now). 

The main reason for the loss was due to the sales addresses having a larger scope than the cambridge shapefiles...
