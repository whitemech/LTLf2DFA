from setuptools import setup, find_packages

with open('README.md') as readme_file:
    readme = readme_file.read()

requirements = ['ply', 'graphviz', 'pydot']

setup(
    author="Francesco Fuggitti",
    author_email='francesco.fuggitti@gmail.com',
    classifiers=[
        'Development Status :: 1 - Planning',
        'Intended Audience :: Education',
        'License :: OSI Approved :: MIT License',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Operating System :: POSIX :: Linux',
    ],
    description="A tool for generating a DFA from an LTLf formula",
    install_requires=requirements,
    license="MIT license",
    long_description=readme,
    long_description_content_type="text/markdown",
    include_package_data=True,
    keywords='ltlf2dfa',
    name='ltlf2dfa',
    packages=find_packages(include=['ltlf2dfa*']),
    url='https://github.com/Francesco17/LTLf2DFA',
    version='0.1.6',
)