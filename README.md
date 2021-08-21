# [GCI-GLOBAL.com](https://gci-global.com)

## This project can be run in 3 ways
1. Full stack locally via Docker
> Note: Database connections are handled via environment variables. You will have to supply your own database and configure ENV variables accordingly.
```bash
docker build -t elm-rust-app . && docker run --env PORT='1234' -p 1234:1234 elm-rust-app # starts this app at http://localhost:1234
```
2. Just the front end
> See dependecies below
```bash
elm-spa server # starts this app at http://localhost:1234
``` 
3. Pushed to Google Cloud
> This is WAY more work, but the `cloudbuild.yaml` is inciuded because I am using it. It can be ignored if using method 1 or 2. 
>[Link your Google Cloud Build with your Git Repo.](https://cloud.google.com/build/docs/automating-builds/run-builds-on-github) Then
>[Configure the `cloudbuild.yaml` with your project-id etc](https://cloud.google.com/build/docs/configuring-builds/create-basic-configuration)
>. Then after struggling for hours its as simple as:
> Note: Database connections are configured with ENV variables, for google cloud those will have to be managed in the google cloud secret manager, then the cloudbuild.yaml will have to be modified to point to those.
```bash
git push origin main
```


## Dependencies for running frontend locally

This project requires the latest LTS version of [Node.js](https://nodejs.org/)

```bash
npm install -g elm elm-spa
```

## Development commands

```bash
elm-spa add    # add a new page to the application
elm-spa build  # production build
elm-spa watch  # runs build as you code (without the server)
```