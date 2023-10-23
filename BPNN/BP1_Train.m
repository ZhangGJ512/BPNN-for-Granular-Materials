% % % 神经网络1：训练应力与应变关系


% % % 获取训练集数据
excel_path1='C:\Users\15430\Desktop\matlab_finally2\TrainData\e';      %应变文件路径（包括名称前缀）
img_path_list1 = dir(strcat(excel_path1,'*.txt'));                     %数据结构体
excel_path2='C:\Users\15430\Desktop\matlab_finally2\TrainData\s';      %应力文件路径（包括名称前缀）
img_path_list2 = dir(strcat(excel_path2,'*.txt'));                     %数据结构体
img_num1=length(img_path_list1);                                       %数据数量


% % % 获取验证集数据
excel_path3='C:\Users\15430\Desktop\matlab_finally2\VerifyData\e';      %应变文件路径（包括名称前缀）
img_path_list3 = dir(strcat(excel_path3,'*.txt'));                      %数据结构体
excel_path4='C:\Users\15430\Desktop\matlab_finally2\VerifyData\s';      %应力文件路径（包括名称前缀）
img_path_list4 = dir(strcat(excel_path4,'*.txt'));                      %数据结构体
img_num2=length(img_path_list3);                                        %数据数量

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
% % % 数据预处理，获得数据数、输入层、隐藏层、输出层的神经元个数
num_train = randperm(img_num1);
num_verify = randperm(img_num2);


for i=1:1:img_num1
    input=readmatrix([excel_path1,num2str(num_train(i)),'.txt'],'Range',[1,1,750,2]);
    output=readmatrix([excel_path2,num2str(num_train(i)),'.txt'],'Range',[1,1,750,2]);     
    
    if i==1
        train_input=input(1:10:end,:);
        train_output=output(1:10:end,:);
    else
        train_input=[train_input;input(1:10:end,:)];
        train_output=[train_output;output(1:10:end,:)];
    end
end

for j=1:1:img_num2
    input=readmatrix([excel_path3,num2str(num_verify(j)),'.txt'],'Range',[1,1,750,2]);
    output=readmatrix([excel_path4,num2str(num_verify(j)),'.txt'],'Range',[1,1,750,2]);
   
    if j==1
        verify_input=input(1:10:end,:);
        verify_output=output(1:10:end,:);
    else
        verify_input=[verify_input;input(1:10:end,:)];
        verify_output=[verify_output;output(1:10:end,:)];
    end
end



T=200000;                    %训练次数
N1=size(train_input,1);      %训练集数据数
N2=size(verify_input,1);     %验证集数据数

d=size(train_input,2);    %输入层：d神经元
e=10;                     %隐含层1：e个神经元
f=10;                     %隐含层2：f个神经元
g=10;                     %隐藏层3：g个神经元
h=size(train_output,2);   %输出层：h个神经元


% % % 定义激活函数
ff = @(x) 1./(1+exp(-x));


% % %定义学习率并初始化权重和偏差
Delta=0.1;         %学习率

W1=rand(d,e);       %输入层到隐藏层1的连接权
W2=rand(e,f);       %隐藏层1到隐藏层2的连接权
W3=rand(f,g);       %隐藏层2到隐藏层3的连接权
W4=rand(g,h);       %隐藏层3到输出层的连接权

Theta1=rand(1,e);      %隐藏层1的阈值
Theta2=rand(1,f);      %隐藏层2的阈值
Theta3=rand(1,g);      %隐藏层3的阈值
Theta4=rand(1,h);      %输出层的阈值

% % %训练
y_record_train=zeros(N1,h);
y_record_verify=zeros(N2,h);

MSE_verify=zeros(N2,1);
MSE_train=zeros(N1,1);
Average_MSE_verify=zeros(T,1);
Average_MSE_train=zeros(T,1);

X_max=max(train_input);
X_min=min(train_input);
Y_max=max(train_output);
Y_min=min(train_output);


for time=1:T
    for k1=1:N1
        %向前传播
        X=train_input(k1,:);Y=train_output(k1,:);
        X_norm=2*(X-X_min)./(X_max-X_min)-1;
        Y_norm=2*(Y-Y_min)./(Y_max-Y_min)-1;

        Alpha1=X_norm*W1;         %隐藏层1输入值
        A1=ff(Alpha1-Theta1);     %隐藏层1输出值

        Alpha2=A1*W2;             %隐藏层2输入值
        A2=ff(Alpha2-Theta2);     %隐藏层2输出值
        
        Alpha3=A2*W3;             %隐藏层3输入值
        A3=ff(Alpha3-Theta3);     %隐藏层3输出值

        Alpha4=A3*W4;              %输出层输入值
        Y_bar=Alpha4-Theta4;       %输出层输出值
        
        MSE_train(k1)=1/2*sum((Y_bar-Y_norm).^2);

        %向后传播
        E1=Y_norm-Y_bar;
        
        dW4=Delta*A3'*E1;            %隐藏层3到输出层的连接权更新增量
        dTheta4=-Delta*E1;           %输出层的阈值更新增量

        E2=A3.*(1-A3).*(E1*W4');

        dW3=Delta*A2'*E2;          %隐藏层2到隐藏层3的连接权更新增量
        dTheta3=-Delta*E2;         %隐藏层3的阈值更新增量

        E3=A2.*(1-A2).*(E2*W3');

        dW2=Delta*A1'*E3;        %隐藏层1到隐藏层2的连接权更新增量
        dTheta2=-Delta*E3;       %隐藏层2的阈值更新增量

        E4=A1.*(1-A1).*(E3*W2');

        dW1=Delta*X_norm'*E4;       %输入层到隐藏层1的连接权更新增量
        dTheta1=-Delta*E4;          %隐藏层1的阈值更新增量


        W1=W1+dW1;       %输入层到隐藏层1的连接权
        W2=W2+dW2;       %隐藏层1到隐藏层2的连接权
        W3=W3+dW3;       %隐藏层2到隐藏层3的连接权
        W4=W4+dW4;       %隐藏层3到输出层的连接权
        
        Theta1=Theta1+dTheta1;      %隐藏层1的阈值
        Theta2=Theta2+dTheta2;      %隐藏层2的阈值
        Theta3=Theta3+dTheta3;      %隐藏层3的阈值
        Theta4=Theta4+dTheta4;      %输出层的阈值
    end
    Average_MSE_train(time)=sum(MSE_train)/N1;
    % 验证集
    for k2=1:N2
        X=verify_input(k2,:);Y=verify_output(k2,:);
        X_norm=2*(X-X_min)./(X_max-X_min)-1;
        Y_norm=2*(Y-Y_min)./(Y_max-Y_min)-1;

        Alpha1=X_norm*W1;         %隐藏层1输入值
        A1=ff(Alpha1-Theta1);     %隐藏层1输出值

        Alpha2=A1*W2;             %隐藏层2输入值
        A2=ff(Alpha2-Theta2);     %隐藏层2输出值
        
        Alpha3=A2*W3;             %隐藏层3输入值
        A3=ff(Alpha3-Theta3);     %隐藏层3输出值

        Alpha4=A3*W4;              %输出层输入值
        Y_bar=Alpha4-Theta4;       %输出层输出值
        
        MSE_verify(k2)=1/2*sum((Y_bar-Y_norm).^2);
    end
    Average_MSE_verify(time)=sum(MSE_verify)/N2;

    % 记录误差最小时的连接权以及阈值
    if time==1
        true_W1=W1;
        true_W2=W2;
        true_W3=W3;
        true_W4=W4;
        true_Theta1=Theta1;
        true_Theta2=Theta2;
        true_Theta3=Theta3;
        true_Theta4=Theta4;
        min_Average_MSE_verify=Average_MSE_verify(time);
        true_time=1;
    else
        if Average_MSE_verify(time)<min_Average_MSE_verify
             true_W1=W1;
             true_W2=W2;
             true_W3=W3;
             true_W4=W4;
             true_Theta1=Theta1;
             true_Theta2=Theta2;
             true_Theta3=Theta3;
             true_Theta4=Theta4;
             true_time=time;
             min_Average_MSE_verify=Average_MSE_verify(time);
        end
    end
end
