# VAqua

An improved native Swing look and feel for macOS.

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.violetlib/vaqua/badge.svg)](https://search.maven.org/artifact/org.violetlib/vaqua)

See [website](https://violetlib.org/vaqua/overview.html) for details and releases.

## Contributing

Setup your development environment by installing the [maven-resolver-ant-tasks](http://maven.apache.org/resolver-ant-tasks/):

```shell
mkdir -p ~/.ant/lib
curl https://repo1.maven.org/maven2/org/apache/maven/resolver/maven-resolver-ant-tasks/1.2.1/maven-resolver-ant-tasks-1.2.1-uber.jar -o ~/.ant/lib/maven-resolver-ant-tasks-1.2.1-uber.jar
```

If you want to check if that is the latest uber-jar, it is found by looking at https://mvnrepository.com/artifact/org.apache.maven.resolver/maven-resolver-ant-tasks, finding the latest version, and then choosing _View All_ under files.

### Building

```shell
ant build
```
