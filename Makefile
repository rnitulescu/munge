## Compile documentation, manual, and package
package: manual
	cd ..; R CMD build munge

manual: doc
	R CMD Rd2pdf . --title="Package \`munge'" --output=./manual.pdf --force

doc:
	R -e "devtools::document()"

