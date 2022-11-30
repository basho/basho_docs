if [[ ! -d "output" ]]
then
    echo "Run this from the root of the repo. i.e. './docker/deploy.beta.sh'"
    exit
fi

# empty output folder
sudo rm -r ./output/*

# generate new content
docker compose -f ./docker/docker-compose.generate-riak-docs-beta.yaml up

# remove target incase it already exists
ssh peter.clark@www.tiot.jp rm -r /var/www/www-tiot-jp-2017-12-13/riak-docs-beta-new

# copy files over
rsync -avzh ./output peter.clark@www.tiot.jp:/var/www/www-tiot-jp-2017-12-13/riak-docs-beta-new

# move files from temp dir to to right place
ssh peter.clark@www.tiot.jp mv /var/www/www-tiot-jp-2017-12-13/riak-docs-beta-new/output/* /var/www/www-tiot-jp-2017-12-13/riak-docs-beta-new

# remove temp dir
ssh peter.clark@www.tiot.jp rm -r /var/www/www-tiot-jp-2017-12-13/riak-docs-beta-new/output

# remove previous old copy
ssh peter.clark@www.tiot.jp rm -r /var/www/www-tiot-jp-2017-12-13/riak-docs-beta-old

# move current to old
ssh peter.clark@www.tiot.jp mv /var/www/www-tiot-jp-2017-12-13/riak-docs-beta /var/www/www-tiot-jp-2017-12-13/riak-docs-beta-old

# move new to current
ssh peter.clark@www.tiot.jp mv /var/www/www-tiot-jp-2017-12-13/riak-docs-beta-new /var/www/www-tiot-jp-2017-12-13/riak-docs-beta
