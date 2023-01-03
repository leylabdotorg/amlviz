# Deploying R Shiny applications to Google Cloud Run

1. Prepare docker image for Shiny applications \
Things needed in docker configuration file
    * `EXPOSE 3838` (Note this is the default port for R Shiny. You can change it to whatever you like. Make sure it matches the below runApp command)
    * `CMD R -e "shiny::runApp('/path/to/shiny/app/', port = 3838, host='0.0.0.0')"` (The host must be 0.0.0.0 in order to communicate GCP)

2. Push Docker image to Artifact registry
    1. If you have not done so already install gcloud CLI [Google docs](https://cloud.google.com/sdk/docs/install)
    2. Login into gcloud with `gcloud auth login`
    3. Follow this [guide](https://cloud.google.com/artifact-registry/docs/docker/store-docker-container-images) on how to setup an artifact registry
    4. Run `gcloud auth configure-docker repository-location-docker.pkg.dev` to allow pushing to Artifact registry \
    Ex: `gcloud auth configure-docker us-central1-docker.pkg.dev`
    5. Build the docker image by running a command with the following form: \
    `docker build . --tag repository-location-docker.pkg.dev/project-name/artifact-registry-repository/image-name:tag-name` \
    Ex: `docker build . --tag us-central1-docker.pkg.dev/leylab/shiny-apps/amlviz:latest`
    6. Push the image to repository with \
    `docker push repository-location-docker.pkg.dev/project-name/artifact-registry-repository/image-name:tag-name` \
    Ex: `docker push us-central1-docker.pkg.dev/leylab/shiny-apps/amlviz:latest`

3. Create service in Google Cloud Platform
    1. Visit [Google Cloud Run portal](https://console.cloud.google.com/run) and click "CREATE SERVICE"
    2. Select the container we previously pushed
    3. Change the "Container port" to whatever you specified in your Dockerfile
    4. Click "CREATE" and a URL will be generated to access your Shiny app
