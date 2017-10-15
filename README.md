# scotty-example

## Quick Start

### Build

该项目使用 stack 工具打包编译，确保已安装 stack 之后再控制台运行如下命令

```
$ stack build
```

### Configuration

使用环境变量 SCOTTY_ENV 可以使配置项目启动环境（default: Dev），环境变量可以使 Prod, Dev, Test 任意一种

```
$ export SCOTTY_ENV=Prod
```

使用环境变量 PORT 可以配置项目启动端口（default: 8080）

```
$ export PORT=1234
```

使用环境变量 MYSQL_HOST、MYSQL_DATABASE、MYSQL_USER、MYSQL_PASSWORD 配置数据库链接（default: localhost, test, root, root）

```
$ export MYSQL_HOST=foo.example.com
```

### Setup

在编译好之后运行下面的命令启动该项目

```
$ stack exec scotty-example
```
