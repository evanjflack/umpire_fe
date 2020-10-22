#!/bin/bash

rm -f README.md
echo \`\`\` >> README.md
tree -L 5 -I 'plots|sandbox|*.log|*.lst|log*|README.md|run_all_scripts.sh|list_dir.sh|model_config' >> README.md
echo \`\`\` >> README.md