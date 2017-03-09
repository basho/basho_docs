#!/bin/sh
function assert_dependencies () {
    assert_dependency_plantuml
}

function assert_dependency_plantuml () {
    if [ -z $(which plantuml) ]; then
        echo "plantuml is required, \`brew install plantuml\` or equivalent" >&2
        exit 1
    fi
}
assert_dependencies

# generate uml diagrams to parallel static dir
for uml_src in $(cat diagrams.lst); do
    uml_dest=$(echo "${uml_src%.*}.png")
    cat $uml_src |plantuml -pipe >../../../static/images/redis/$uml_dest
done
