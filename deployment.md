# Deployment

Automated tests for data warehouse should run
1. When code changes have been commited
2. When data load jobs have taken place

To run our tests we will deploy them to a docker container which can deployed using a services such as 
Azure Container Registry (to store the container image)

Azure Container Apps (to run the container image)

Other potential alternatives to Azure Container Apps include
Azure Batch
https://azure.microsoft.com/en-us/products/batch

Azure App Service

Azure Container Instance

The test results report can be written to static HTML files and saved to Azure Blob Storage which can act act as a fast, secure and cost effective method for hosting HTML pages with the relevant team members.



