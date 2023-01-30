#include "mainwindow.h"
#include "./ui_mainwindow.h"
#include <QFileSystemModel>
MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    QFileSystemModel *modelo=new QFileSystemModel(this);
    modelo->setRootPath(QDir::rootPath());
    //modelo->setRootIndex(dirModel->index("/Users"));
    ui->treeView->setModel(modelo);
}

MainWindow::~MainWindow()
{
    delete ui;
}

