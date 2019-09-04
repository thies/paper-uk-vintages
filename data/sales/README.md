## Transaction data

Download sales data from land registry (all sales, UK wide, since 1995) and then reduce sample to Cambridge only.
File: [sales_cambridge.csv](https://www.dropbox.com/s/vsrtks68vwxcd24/sales_cambridge.csv?dl=0)

```bash
curl -O http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv
grep '"CAMBRIDGE"' pp-complete.csv > sales_cambridge.csv
```

