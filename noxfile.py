import nox

@nox.session
def lint(session):
    session.install("-r", "requirements.txt")
    session.run("flake8", "main2.py")

# @nox.session
# def test(session):
#    session.run("pytest", "tests")
