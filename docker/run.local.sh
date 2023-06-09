if [[ ! -d "output" ]]
then
    echo "Run this from the root of the repo. i.e. './docker/run.local.sh'"
    exit
fi

# empty output folder
sudo rm -r ./output/*

# generate new content
docker compose -f ./docker/docker-compose.localhost-preview.yaml up
