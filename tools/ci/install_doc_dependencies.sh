#  Set up environment needed for Sphinx to build docs
sudo apt-get update
sudo apt-get install texlive-latex-recommended
sudo apt-get install texlive-latex-extra
sudo apt-get install texlive-fonts-recommended
sudo apt-get install latexmk
pip install sphinx
pip install sphinx_rtd_theme
pip install sphinxcontrib-bibtex