if [[ ! -d "output" ]]
then
    echo "Run this from the root of the repo. i.e. './docker/deploy.root.sh'"
    exit
fi

# empty output folder
sudo rm -r ./output/*

# generate new content
docker compose -f ./docker/docker-compose.generate-root.yaml up

# remove target incase it already exists
ssh ubuntu@docs.riak.info sudo rm -r /var/www/docs.riak.info-new

# make target
ssh ubuntu@docs.riak.info sudo mkdir /var/www/docs.riak.info-new
ssh ubuntu@docs.riak.info chown -R ubuntu:www-data /var/www/docs.riak.info

# copy files over
rsync -avzh ./output ubuntu@docs.riak.info:/var/www/docs.riak.info-new

# move files from temp dir to to right place
ssh ubuntu@docs.riak.info mv /var/www/docks.riak.info-new/output/* /var/www/docs.riak.info-new

# remove temp dir
ssh ubuntu@docs.riak.info rm -r /var/www/docs.riak.info-new/output

# remove previous old copy
ssh ubuntu@docs.riak.info rm -r /var/www/docs.riak.info-old

# move current to old
ssh ubuntu@docs.riak.info mv /var/www/docs.riak.info /var/www/docs.riak-info-old

# move new to current
ssh ubuntu@docs.riak.info mv /var/www/docs.riak.info-new /var/www/docs.riak.info

# reset permissions again to be safe
ssh ubuntu@docs.riak.info chown -R ubuntu:www-data /var/www/docs.riak.info
