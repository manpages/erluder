#!/bin/bash
mkdir metabb

if [[ $EUID -ne 0 ]]; then
	echo "run me as root plz"
	echo "kthxbye"
	exit 1
fi

echo "icanhaz: git, hg-mercurial, erlang, gcc, make, autoconf"

echo "YAWS! incoming!"
git clone git://github.com/klacke/yaws.git
cd yaws
autoconf; ./configure; make; make local_install
cd ..
wget http://memoricide.very.lv/yaws.conf
mkdir logs

#echo "Moustache or beard?"
#git clone https://github.com/mojombo/mustache.erl.git ./mustache

echo "Radiation!"
hg clone https://bitbucket.org/verypositive/fission
cd fission
make
cd ..
mkdir data

echo "I think, I'm funny. You should just ignore me."
touch .gitignore
echo "*" > .gitignore
echo "!/env.sh" >> .gitignore
echo "!/metabb/" >> .gitignore
echo "!/eunit/" >> .gitignore

touch ./metabb/.gitignore
echo "!*" > ./metabb/.gitignore
echo "public/files/" >> ./metabb/.gitignore

touch Emakefile
echo "{\"fission/src/*\", [{outdir, \"fission/ebin\"}]}." > Emakefile
echo "{\"metabb/src/*\", [{outdir, \"metabb/ebin\"}]}." >> Emakefile
echo "{\"eunit/src/*\", [{outdir, \"eunit/ebin\"}]}." >> Emakefile
#echo "{\"mustache/src/*\", [{outdir, \"mustache/ebin\"}]}." >> Emakefile

touch erl.sh
echo "erl -name wip@somehost -pa fission/ebin/ fisbb/ebin/ yaws/ebin/ mustache/ebin" > erl.sh
chmod 775 ./erl.sh

echo ""
echo "I'm tired. do 'chgrp -R <group> .' and 'chown -R <user> .' yourself"
echo "And you might like to edit ./yaws.conf and ./erl.sh"
echo "Put something like metabb@<your_hostname> after -name key in ./erl.sh"
echo ""
echo "To get it finally working run erl.sh"
echo "> application:start(yaws)."
echo "> application:start(fission)."
echo "If it is the first run, also do"
echo "> fission:initialize(node())."
 
