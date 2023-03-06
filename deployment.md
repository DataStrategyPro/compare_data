# Deployment

The ability to run tests locally on a laptop provides a lot of flexibility when there is a lack of available infrastructure. 

Cloud infrastructure is now extremely affordable and is charged at cents per hour of processing time. 

Moving processing to the cloud also means improvements in 

 - Infrastructure robustness
 - Process robustness
 - Data security
 - Organisational access
 - Reduced data transfer over slow networks
 - Ability to schedule tasks
 - Ability to run based on event triggers 

Automated tests for data warehouse should run

1. When code changes have been commited
2. When data load jobs have taken place

To run our tests we will deploy them to a docker container which can deployed using a services such as 
Azure Container Registry (to store the container image)
Azure Container Apps (to run the container image)
Azure Functions (to trigger runs)
Azure Blob storage (to store reference files and test results)

Azure has a number of services for running containers including
Azure Container Apps
Azure Batch
Azure App Service
Azure Container Instance

The test results report can be written to static HTML files and saved to Azure Blob Storage which can act act as a fast, secure and cost effective method for hosting HTML pages with the relevant team members.



