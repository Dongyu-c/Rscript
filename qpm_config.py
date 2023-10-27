import pandas as pd
import pyreadr
import numpy as np
import copy

import os, shutil   

from statsmodels.stats.proportion import proportions_ztest
import matplotlib.pyplot as plt
from PIL import Image                                                                                
from plotnine import ggplot,annotate,position_dodge,geom_col,scale_fill_discrete,position_stack, geom_text,xlab,ggtitle,aes, geom_line,geom_boxplot,geom_point,facet_wrap,geom_hline,xlab,theme,scale_x_discrete,element_text

import PyPDF2
from PyPDF2 import PdfFileReader, PdfFileWriter
from PyPDF2 import PageObject

import math
from plotly.subplots import make_subplots
import plotly.graph_objects as go
import plotly.offline as offline
import matplotlib.pyplot as plt

from datetime import datetime
import pytz

def isNaN(string):
    return string != string

def pre_prpcessing(rawdata,parameter):
    """
    Before doing config analysis,find the part_num and the newest group_num for a certain parameter in the dataframe. 
    Input: 
        rawdata is dataframe with commodity data. 
        parameter is the yaxis for config analysis. eg:parameter='FLANGE_HT_PROJECTION'
    Output: 
        partn_sup_groupnum: dict structure

    """

    rawdata1 = copy.deepcopy(rawdata)
    
    rawdata1['GROUP_NUM1'] = [str(num) for num in rawdata1['GROUP_NUM']]
    
    partn_sup_groupnum=dict()
    partn_groupnum=dict()
    for partnum in rawdata1['PART_NUM'].unique():
        raw_part=rawdata1[rawdata1['PART_NUM']==partnum]
        for sup in raw_part['SUPPLIER_NAME'].unique():
            partn_sup_groupnum.update({partnum:{}})
            df_motor=raw_part[raw_part['SUPPLIER_NAME']==sup]
            try:
                ds=df_motor.groupby(['GROUP_NUM1'],as_index=False).count()[['GROUP_NUM1',parameter]].sort_values(by='GROUP_NUM1')
            except:
                print('GROUP_NUM contains invalid character.')
                continue
            groupnum=ds.tail(1)['GROUP_NUM1'].values[0] ###the newest group_num
            partn_groupnum.update({sup:groupnum})
            partn_sup_groupnum[partnum].update(partn_groupnum)
    return partn_sup_groupnum

def pre_prpcessing_motor(rawdata,parameter):
    """
    Before doing config analysis,find the part_num and the newest group_num for a certain parameter in the dataframe. 
    Input: 
        rawdata is dataframe with commodity data. 
        parameter is the yaxis for config analysis. eg:parameter='FLANGE_HT_PROJECTION'
    Output: 
        partn_sup_groupnum: dict structure

    """
    rawdata1 = copy.deepcopy(rawdata)
    
    rawdata1['GROUP_NUM1'] = [int(str(num)[3]+str(num)[1]+str(num)[2]+str(num)[0]) for num in rawdata1['GROUP_NUM']]

    partn_sup_groupnum=dict()
    partn_groupnum=dict()
    for partnum in rawdata1['PART_NUM'].unique():
        raw_part=rawdata1[rawdata1['PART_NUM']==partnum]
        for sup in raw_part['SUPPLIER_NAME'].unique():
            partn_sup_groupnum.update({partnum:{}})
            df_motor=raw_part[raw_part['SUPPLIER_NAME']==sup]
            try:
                ds=df_motor.groupby(['GROUP_NUM1'],as_index=False).count()[['GROUP_NUM1',parameter]].sort_values(by='GROUP_NUM1')
            except:
                print('GROUP_NUM contains invalid character.')
                continue
            groupnum=ds.tail(1)['GROUP_NUM1'].values[0] ###the newest group_num
            partn_groupnum.update({sup:groupnum})
            partn_sup_groupnum[partnum].update(partn_groupnum)
    return partn_sup_groupnum

def cal_mean_shift(df_t,df_b,parameter):
    mean_shift=df_t[parameter].mean()-df_b[parameter].mean()
    return mean_shift

def cal_var_shift(df_t,df_b,parameter):
    var_shift=(df_t[parameter].var()+df_t[parameter].mean()**2)-(df_b[parameter].var()+df_b[parameter].mean()**2)
    return var_shift

def cal_attr_ratio(df_b,df_t,attr):

    """ new data's attr layer need in the baseline 
    Base line ratio is only calculate these attr 
    """
    df_b.dropna(subset=[attr],inplace=True)
    df_t.dropna(subset=[attr],inplace=True)
    t_ratio=pd.crosstab(df_t[attr],df_t['status'], normalize='columns').reset_index() 
    df_b1=df_b[df_b[attr].isin(t_ratio[attr].unique())]
    b_ratio=pd.crosstab(df_b1[attr],df_b1['status'], normalize='columns').reset_index() 
    try:
        df_ratio=pd.merge(b_ratio,t_ratio,on=[attr],how='outer')
        df_ratio.fillna(0,inplace=True)
        df_ratio.set_index(attr,inplace=True)
    except:
        df_t[attr].unique().tolist()
        print('attribute changed,{0} has {1}, but not included in the baseline'.format(attr,str(df_t[attr].unique())))
    
    return df_ratio,df_b1

def cal_contribution(df_t,df_b,parameter,attr,attr_layer,shift,baseline_ratio,trigger_ratio,con):
    #attr_layer='R',shift use mean_shift,var_shift
    if con == "var":
        c=(trigger_ratio*(df_t[df_t[attr]==attr_layer].mean()[parameter]**2+df_t[df_t[attr]==attr_layer].var()[parameter])-(baseline_ratio*((df_b[df_b[attr]==attr_layer].mean()[parameter])**2+df_b[df_b[attr]==attr_layer].var()[parameter])))/shift
    else:
        c=(trigger_ratio*df_t[df_t[attr]==attr_layer].mean()[parameter]-baseline_ratio*df_b[df_b[attr]==attr_layer].mean()[parameter])/shift
    return ((c))

from scipy.stats import chi2
from scipy.optimize import fsolve

def lvl_two_shift(parameter,attr,df_b,df_t):
    try:
        df_ratio,df_b1=cal_attr_ratio(df_b,df_t,attr)# baseline and Newdata need the same attr
        mean_shift=cal_mean_shift(df_t,df_b1,parameter)
        var_shift=cal_var_shift(df_t,df_b1,parameter)
        c_m=dict()
        c_v=dict()
        
        for ind in df_ratio.index:
            attr_layer=len(df_ratio)
            baseline_ratio=df_ratio.loc[ind,'BASELINE']
            trigger_ratio=df_ratio.loc[ind,'NEWDATA']
            c_mean=cal_contribution(df_t,df_b1,parameter,attr,ind,mean_shift,baseline_ratio,trigger_ratio,"mean")
            c_var=cal_contribution(df_t,df_b1,parameter,attr,ind,var_shift,baseline_ratio,trigger_ratio,"var")
            c_m.update({ind:c_mean})
            c_v.update({ind:c_var})
    except:
        print ('lvl_two_shift is error because of attribute changed')
        
    return c_m,c_v

def gen_result(m_shift,v_shift):
    final=[]
    print(f"m_shift: {m_shift}")
    for attr in m_shift.keys():
        print(f"attr: {attr}")
        A=pd.Series(m_shift[attr]).dropna().to_frame()
        B=pd.Series(v_shift[attr]).dropna().to_frame()
        A.columns=['Mean Shift']
        B.columns=['Variance Shift']
        A.reset_index(inplace=True)
        B.reset_index(inplace=True)
        data_set=pd.merge(A,B,how='inner',on='index')
        data_set.rename(columns={'index':'atr'},inplace=True)
        data_set['attribute']=attr+'_ '+data_set['atr'].astype('str')
        data_set['col']=attr
        m_threshold=lvl_two_threshold(data_set,"mean")
        v_threshold=lvl_two_threshold(data_set,"var")
        data_set['Mean Threshold']=m_threshold[0]
        data_set['Variance Threshold']=v_threshold[0]
        final.append(data_set)
    final_result=pd.concat(final)
    print("Final result is ",final_result)
    return final_result



def lvl_two_threshold(data_set,con):
    df = len(data_set)-1
    vals=chi2.ppf([0.05],df)
    if con == "var":
        data_set['Variance Shift1']=[i if i >0 else 0 for i in data_set['Variance Shift'] ]
        func=lambda x:vals-sum((data_set['Variance Shift']-x)**2/x)
    else :
        data_set['Mean Shift1']=[i if i >0 else 0 for i in data_set['Mean Shift'] ]
        func=lambda x:vals-sum((data_set['Mean Shift1']-x)**2/x)
    threshold =fsolve(func,[10])
    return threshold

import numpy as np

def generate_newest_boxplot_motor(df_motor,df_motor3,zaxis,yaxis,fill_axis,title,part_num):
    
    sample = copy.deepcopy(df_motor)

    z_axis = zaxis+' '
    
    real_list = [str(i) for i in list(map(int, sample[zaxis]))]
    
    sample[z_axis] = real_list

    mean_sample = sample.groupby([zaxis,'Filter'])[yaxis].mean().reset_index()
    
    mean_sample[z_axis] = [str(i) for i in list(map(int, mean_sample[zaxis]))]

    
    attr_list = mean_sample[zaxis].tolist()
    filter_list = mean_sample['Filter'].tolist()
    
    mean_sample['Real_Filter'] = [(str(attr_list[i])+'\n'+str(filter_list[i])) for i in range(len(mean_sample['Filter']))]

    ###
    mean_sample1 = mean_sample.sort_values(by=[z_axis]).reset_index()
    sample1 = sample.sort_values(by=[z_axis]).reset_index()

    sample3 = copy.deepcopy(df_motor3)
    
    sample3[z_axis] =[str(i) for i in list(map(int, sample3[zaxis]))]
    
    
    attr_dif1 = ((sample[sample['Filter']=='NEW'][yaxis].mean() - sample[sample['Filter']=='BSL'][yaxis].mean()) / sample[yaxis].std())
    attr_dif1 = abs(attr_dif1)
    attr_dif = format(attr_dif1, '.4f')
    
    new_data_dif1 = ((sample3[sample3['Filter']=='NEW'][yaxis].mean() - sample3[sample3['Filter']=='BSL'][yaxis].mean()) / sample3[yaxis].std())
    new_data_dif1 = abs(new_data_dif1)
    new_data_dif = format(new_data_dif1, '.4f')
    
    x_anno = mean_sample1['Real_Filter'].tolist()[1]
    sample3['Filter'].unique()[0]
    label1 = 'Greatest Contribution Attribute\nGreatest Attribute Diff\nNew Lot Diff\nSample Qty'
    label1  = label1+ '\n' +z_axis+ '_'+str(sample3[zaxis].unique().tolist()[0])
    label1 = label1 + '\n' + str(new_data_dif)
    
    greatst_count=len(sample3[sample3['Filter']=='NEW'][yaxis])
    baseline_count=len(sample3[sample3['Filter']=='BSL'][yaxis])
    count=np.array([greatst_count,baseline_count])
    nobs=np.array([len(sample[sample['Filter']=='NEW'][yaxis]),len(sample[sample['Filter']=='BSL'][yaxis])])
    z_score, p_value = proportions_ztest(count, nobs)#,alternative='two-sided')
    print('z is ',z_score,' and p is',p_value)
    p_value = format(p_value, '.4f')
    z_score = format(z_score, '.4f')
    
    Abp = []
    Anp = []
    
    label1 = label1 + '\n'+str(attr_dif)
    label1  = label1 + '\n' + str(greatst_count)+ '/' +str(len(sample[sample['Filter']=='NEW'][yaxis]))+';'+ str(baseline_count) + '/' + str(len(sample[sample['Filter']=='BSL'][yaxis]))
    
    aplot = ggplot(aes(x='Real_Filter',y = yaxis)) +\
    geom_boxplot(sample1,fill = '#C6E2FF') +\
    scale_x_discrete(labels=lambda X: [i[len(i)-3:]+'\n\n\n'+i[:len(i)-4] for i in X])+\
    scale_fill_discrete(labels = ['BSL','NEW'])+\
    xlab(z_axis)+\
    geom_boxplot(sample3,fill = '#EE9999') +\
    geom_point(mean_sample1,aes(group = 4), shape ='+', size=5 )+\
    geom_line(mean_sample1,aes(group = 4), color = 'pink', size=1 )+\
    geom_text(mean_sample1,aes(group = 4,label = "{0}".format(yaxis)), format_string='{:.4f}', va="bottom",ha = "left")+\
    theme(panel_spacing = 0.45)+\
    ggtitle(title)
    
    if len(df_motor['Real_Filter'].unique().tolist()) > 45:
        aplot = aplot + theme(figure_size = (36,9),axis_text_x=element_text(hjust=1))
    elif len(df_motor['Real_Filter'].unique().tolist()) > 25:
        aplot = aplot + theme(figure_size = (20,8),axis_text_x=element_text(hjust=1))
    else:
        aplot = aplot + theme(figure_size = (10,7),axis_text_x=element_text(hjust=1))
        
    lowest = []   
    lowest.append(sample[sample['Filter']=='NEW'][yaxis].min())
    if (part_num in SPEC[SPEC['COMMODITY'] == 'MO']['PART_NUM'].unique().tolist()):
        SPEC1 = SPEC[SPEC['COMMODITY'] == 'MO']
        SPEC2 = SPEC1[SPEC1['PART_NUM'] == part_num]
        if(yaxis in SPEC2['QPM_PARAMETER'].unique().tolist()):
            SPEC3 = SPEC2[SPEC2['QPM_PARAMETER'] == yaxis]
            TARGET = SPEC3['Target'].unique().tolist()[0]
            USL = SPEC3['USL'].unique().tolist()[0]
            LSL = SPEC3['LSL'].unique().tolist()[0]
            if not isNaN(TARGET):
                aplot = aplot + geom_hline(yintercept = float(TARGET), color='green')
                lowest.append(float(TARGET))
            if not isNaN(USL):
                aplot = aplot + geom_hline(yintercept = float(USL), color='red')
                lowest.append(float(USL))
            if not isNaN(LSL):
                aplot = aplot + geom_hline(yintercept = float(LSL), color='red')
                lowest.append(float(LSL))
        
    greatst_count=len(sample3[sample3['Filter']=='NEW'][yaxis])
    baseline_count=len(sample3[sample3['Filter']=='BSL'][yaxis])
    Gbp = str(greatst_count)+ '/' +str(len(sample[sample['Filter']=='NEW'][yaxis]))
    Gnp = str(baseline_count) + '/' + str(len(sample[sample['Filter']=='BSL'][yaxis]))
    sup = df_motor['SUPPLIER_NAME'].unique().tolist()
    
    one_point_five = False
    if abs(float(new_data_dif)) >= 1.5:
        if float(greatst_count) >= 15:
            if float(baseline_count)>=15:
                one_point_five = True 
    return ([aplot,[yaxis,zaxis,new_data_dif,attr_dif,Gbp,Gnp,sample[zaxis].unique().tolist(),Abp,Anp,sup[0]], one_point_five,label1])

def generate_newest_boxplot(df_motor,df_motor3,zaxis,yaxis,fill_axis,title,part_num):
    sample = copy.deepcopy(df_motor)

    z_axis = zaxis+' '
    
    real_list = [str(i) for i in list(map(str, sample[zaxis]))]

    sample[z_axis] = real_list

    mean_sample = sample.groupby([zaxis,'Filter'])[yaxis].mean().reset_index()
    
    mean_sample[z_axis] = [str(i) for i in list(map(str, mean_sample[zaxis]))]
    
    attr_list = mean_sample[zaxis].tolist()
    filter_list = mean_sample['Filter'].tolist()

    mean_sample['Real_Filter'] = [(str(attr_list[i])+'\n'+str(filter_list[i])) for i in range(len(mean_sample['Filter']))]
    
    ###
    mean_sample1 = mean_sample.sort_values(by=[z_axis]).reset_index()
    sample1 = sample.sort_values(by=[z_axis]).reset_index()
    
    sample3 = copy.deepcopy(df_motor3)
    
    real_list = [str(i) for i in list(map(str, sample3[zaxis]))]
        
    sample3[z_axis] = real_list
    
    attr_dif1 = abs((sample[sample['Filter']=='NEW'][yaxis].mean() - sample[sample['Filter']=='BSL'][yaxis].mean()) / sample[yaxis].std())
    attr_dif = format(attr_dif1, '.4f')
    
    new_data_dif1 = abs((sample3[sample3['Filter']=='NEW'][yaxis].mean() - sample3[sample3['Filter']=='BSL'][yaxis].mean()) / sample3[yaxis].std())
    
    new_data_dif = format(new_data_dif1, '.4f')
    
    x_anno = mean_sample1['Real_Filter'].tolist()[1]
    
    label1 = 'Greatest Contribution Attribute\nGreatest Attribute Diff\nNew Lot Diff\nSample Qty'
    label1  = label1+ '\n' +z_axis+ '_'+str(sample3[zaxis].unique().tolist()[0])
    label1 = label1 + '\n' + str(new_data_dif)
    
    greatst_count=len(sample3[sample3['Filter']=='NEW'][yaxis])
    baseline_count=len(sample3[sample3['Filter']=='BSL'][yaxis])
    count=np.array([greatst_count,baseline_count])
    nobs=np.array([len(sample[sample['Filter']=='NEW'][yaxis]),len(sample[sample['Filter']=='BSL'][yaxis])])
    z_score, p_value = proportions_ztest(count, nobs)#,alternative='two-sided')
    print('z is ',z_score,' and p is',p_value)
    p_value = format(p_value, '.4f')
    z_score = format(z_score, '.4f')

    Abp = []
    Anp = []
    
    label1 = label1 + '\n'+str(attr_dif)
    label1  = label1 + '\n' + str(greatst_count)+ '/' +str(len(sample[sample['Filter']=='NEW'][yaxis]))+';'+ str(baseline_count) + '/' + str(len(sample[sample['Filter']=='BSL'][yaxis]))
    
    aplot = ggplot(aes(x='Real_Filter',y = yaxis)) +\
    geom_boxplot(sample1,fill = '#C6E2FF') +\
    scale_x_discrete(labels=lambda X: [i[len(i)-3:]+'\n\n\n'+i[:len(i)-4] for i in X])+\
    scale_fill_discrete(labels = ['BSL','NEW'])+\
    xlab(z_axis)+\
    geom_boxplot(sample3,fill = '#EE9999') +\
    geom_point(mean_sample1,aes(group = 4), shape ='+', size=5 )+\
    geom_line(mean_sample1,aes(group = 4), color = 'pink', size=1 )+\
    geom_text(mean_sample1,aes(group = 4,label = "{0}".format(yaxis)),format_string='{:.4f}', va="bottom",ha = "left")+\
    theme(panel_spacing = 0.45)+\
    ggtitle(title)
    
    if len(df_motor['Real_Filter'].unique().tolist()) > 45:
        aplot = aplot + theme(figure_size = (36,9),axis_text_x=element_text(hjust=1))
    elif len(df_motor['Real_Filter'].unique().tolist()) > 30:
        aplot = aplot + theme(figure_size = (20,8),axis_text_x=element_text(hjust=1))
    else:
        aplot = aplot + theme(figure_size = (10,7),axis_text_x=element_text(hjust=1))
    
    nums = (sample1[sample1['Real_Filter'] == x_anno][yaxis])
    lowest = np.percentile(nums, (0,75),interpolation = 'midpoint')[0]
    
    if (part_num in SPEC[SPEC['COMMODITY'] == 'MO']['PART_NUM'].unique().tolist()):
        SPEC1 = SPEC[SPEC['COMMODITY'] == 'MO']
        SPEC2 = SPEC1[SPEC1['PART_NUM'] == part_num]
        if(yaxis in SPEC2['QPM_PARAMETER'].unique().tolist()):
            SPEC3 = SPEC2[SPEC2['QPM_PARAMETER'] == yaxis]
            TARGET = SPEC3['Target'].unique().tolist()[0]
            USL = SPEC3['USL'].unique().tolist()[0]
            LSL = SPEC3['LSL'].unique().tolist()[0]
            if not isNaN(TARGET):
                aplot = aplot + geom_hline(yintercept = float(TARGET), color='green')
                
            if not isNaN(USL):
                aplot = aplot + geom_hline(yintercept = float(USL), color='red')
                
            if not isNaN(LSL):
                aplot = aplot + geom_hline(yintercept = float(LSL), color='red')
                
    sup = df_motor['SUPPLIER_NAME'].unique().tolist()
    greatst_count=len(sample3[sample3['Filter']=='NEW'][yaxis])
    baseline_count=len(sample3[sample3['Filter']=='BSL'][yaxis])
    Gbp = str(greatst_count)+ '/' +str(len(sample[sample['Filter']=='NEW'][yaxis]))
    Gnp = str(baseline_count) + '/' + str(len(sample[sample['Filter']=='BSL'][yaxis]))
    
    
    one_point_five = False
    if abs(float(new_data_dif)) >=1.5:
        if float(greatst_count) >= 15:
            if float(baseline_count)>=15:
                one_point_five = True 
    
    return ([aplot,[yaxis,zaxis,new_data_dif,attr_dif,Gbp,Gnp,sample[zaxis].unique().tolist(),Abp,Anp,sup[0]],one_point_five,label1])

def plot1(input_commodity_name,loc,df_motor,parameter,partnum,new_groupnum,final_result,title,shift_layer='Mean Shift',threshold_layer='Mean Threshold'):
    
    from PyPDF2 import PdfFileReader, PdfFileMerger
    
    if shift_layer=='Mean Shift':
        final_result.sort_values(by='mean_thresold_ratio',ascending=False,inplace=True)
    else:
        final_result.sort_values(by='variance_thresold_ratio',ascending=False,inplace=True)
    attr_fac=final_result[final_result[shift_layer]-final_result[threshold_layer]>0]['col'].unique()
    ftitle=title+'_'+shift_layer
    
    output = PdfFileMerger()
    one_point_five = False
    for i,attr in enumerate(attr_fac):
        print('attr is ',attr)
        data_set=final_result[final_result['col']==attr]
        greatest_line = data_set.atr.tolist()[0]
        print('data_set.atr.tolist() is ',data_set.atr.tolist())
        fig=go.Figure(data=[go.Bar(
        x=data_set.attribute.tolist(),
        y=data_set[shift_layer].tolist()
         ,name=shift_layer
        ),
        go.Scatter(
        x=data_set.attribute.tolist(),
        y=data_set[threshold_layer].tolist(),
        name=threshold_layer
        )])

        fig.update_layout(title=ftitle, height=700, width=1000, showlegend=True)
        # fig.show()
        fig.write_image(f'pdf_file.pdf')
        pdf_file = PdfFileReader('pdf_file.pdf')
        output.append(pdf_file) 
 
        df_motor_new = copy.deepcopy(df_motor)
        fill_axis = []
        for item in df_motor_new['GROUP_NUM']:
            if item  == new_groupnum:
                fill_axis.append('NEW')
            else:
                fill_axis.append('BSL')
        df_motor_new['Filter'] = fill_axis

        
        attr_list = df_motor_new[attr].tolist()
        filter_list = df_motor_new['Filter'].tolist()
        
        df_motor_new['Real_Filter'] = [(str(attr_list[i])+'\n'+str(filter_list[i])) for i in range(len(df_motor_new['Filter']))]
        
        df_motor_new_base = df_motor_new[df_motor_new['Filter'] =='BSL' ]
        df_motor_new_new = df_motor_new[df_motor_new['Filter'] =='NEW' ]
        df_motor2 = pd.DataFrame(columns=df_motor.columns)
        df_motor3 = pd.DataFrame(columns=df_motor.columns)
        greatest_item = 0
        print(df_motor_new[attr].unique().tolist())
        for item in data_set.atr.tolist():  
            df_motor2 = df_motor2.append(df_motor_new_base[df_motor_new_base[attr] == item],ignore_index = True)
            df_motor2 = df_motor2.append(df_motor_new_new[df_motor_new_new[attr] == item],ignore_index = True)
            if greatest_item == 0:
                df_motor3 = df_motor3.append(df_motor_new_base[df_motor_new_base[attr] == item],ignore_index = True)
                df_motor3 = df_motor3.append(df_motor_new_new[df_motor_new_new[attr] == item],ignore_index = True)
                greatest_item = 1
                
        title2 = 'GROUP_NUM '+str(new_groupnum)
        if input_commodity_name == 'MOTOR':
            result = generate_newest_boxplot_motor(df_motor2,df_motor3,attr,parameter,'Filter',title2,str(partnum))
        else:
            result = generate_newest_boxplot(df_motor2,df_motor3,attr,parameter,'Filter',title2,str(partnum))
        aplot = result[0]
        aplot.save(f'pdf_file2.pdf',limitsize=False)
        pdf_file = PdfFileReader('pdf_file2.pdf')
        
        output_list = result[3].split('\n')
        
        three_member = []
        cell_text = []
        for i in output_list:
            if len(three_member) <= 3:
                three_member.append(i)
            else:
                cell_text.append(three_member)
                three_member = []
                three_member.append(i)
        while len(three_member) <= 3:
            three_member.append('')
        cell_text.append(three_member)

        fig, ax = plt.subplots()
        plt.rcParams["figure.figsize"] = [9.00, 1.50]
        ax.axis('off')
        ax.axis('tight')

        ax.table(cellText=cell_text,
                 loc = 'center',
                 cellLoc = 'center'
                 )
        
        fig.savefig('temp.pdf', dpi=None, pad_inches = 0)
        #plt.show()
        
        temp_pdf_file = PdfFileReader('temp.pdf')
        
        page_1 = pdf_file.getPage(0)
        page_2 = temp_pdf_file.getPage(0)
        
        ratio1 = float(752)/float(page_1.mediaBox.getWidth())
        ratio2 = float(752)/float(page_2.mediaBox.getWidth())
        page_1.scale(ratio1,ratio1)
        page_2.scale(ratio2,ratio2)
        
        #Creating a new file double the size of the original
        translated_page = PageObject.createBlankPage(None, page_1.mediaBox.getWidth(), page_1.mediaBox.getHeight()+page_2.mediaBox.getHeight())
        #print('page_1.mediaBox has height ', page_1.mediaBox.getHeight())
        #print('page_2.mediaBox has height ', page_2.mediaBox.getHeight())
        #Adding the pages to the new empty page
        
        #print("type(page_2.mediaBox.getHeight())")
        #print(type(page_2.mediaBox.getHeight()))
        
        translated_page.mergeScaledTranslatedPage(page_1, 1, 0, float(page_2.mediaBox.getHeight()))
        translated_page.mergePage(page_2)
        print('translated_page.mediaBox has height ',translated_page.mediaBox.getHeight())
        writer = PdfFileWriter()
        writer.addPage(translated_page)

        with open('out.pdf', 'wb') as f:
            writer.write(f)
        out_pdf_file = PdfFileReader('out.pdf')
        output.append(out_pdf_file)

        plt.close(fig) 

        if result[2]:
            one_point_five = True

    ftitle = ftitle.replace('/', '-')
    
    print("CHECKING : loc+'/1point5diff'")
    print(loc+'/1point5diff')
    print("CHECKING : loc+'/'+ftitle+'.pdf'")
    print(loc+'/'+ftitle+'.pdf')
    
    with open(loc+'/'+ftitle+".pdf", "wb") as output_stream:
        output.write(output_stream)
    if one_point_five:     
        try:
            os.mkdir(loc+'/1point5diff')
        except:
            print('Folder exists.')
        try:
            shutil.copyfile(loc+'/'+ftitle+'.pdf',loc+'/1point5diff/'+ftitle+".pdf")
        except:
            print('File '+ftitle+".pdf"+' exists.')
   
    return result[1]

def get_files(path):
    for file in os.listdir(path):
        if os.path.isfile(os.path.join(path, file)):
            yield file

def find_commodity_name(file_name):
    commodity_name = ''
    is_stop = 0
    start_char = len('INP_QPM_DASHBOARD_')
    for i in range(start_char,len(file_name)):
        if file_name[i].isalpha() and is_stop == 0:
            commodity_name = commodity_name + file_name[i]
        else:
            is_stop = 1
    return commodity_name

def find_new_size(pdf_readin):
    for i in range(len(pdf_readin)):
        if pdf_readin[i] == '/':
            loc = i-1
            break
    new_size = ''
    while(pdf_readin[loc] != '\n'):
        new_size = pdf_readin[loc] + new_size 
        loc = loc -1
    return (int(new_size))
    
INPUT_PATH = '/seamnt/sasdata/sasdata/ecube/results/E1118762/'
OUTPUT_PATH = INPUT_PATH + '/QPM_CONFIG_RESULTS/'
PER_UPLOAD_PATH = '/mnt/okmpna3/ecube_upload/534180/'

if(os.path.exists(OUTPUT_PATH) == False):
    os.makedirs(OUTPUT_PATH)
    
os.chdir(INPUT_PATH)

SPEC = pd.read_csv('QPM_SPEC.csv')

if __name__=='__main__':

    qpm_config_process = ['INP_QPM_DASHBOARD_ACT.csv', 'INP_QPM_DASHBOARD_CLAMP.csv', 'INP_QPM_DASHBOARD_DSP.csv', 'INP_QPM_DASHBOARD_MOTOR.csv', 'INP_QPM_DASHBOARD_VCMA.csv', 'INP_QPM_DASHBOARD_STMCV.csv', 'INP_QPM_DASHBOARD_SPACER.csv', 'INP_QPM_DASHBOARD_RAMP.csv'];
    
    for f in qpm_config_process:
        input_file_name = f
        input_commodity_name = f.replace("INP_QPM_DASHBOARD_", "").replace(".csv", '')
        
        # import datetime
        # date_object = datetime.date.today()
        
        date_object = datetime.now(pytz.timezone('Asia/Shanghai')).date()
        
        loc = OUTPUT_PATH+input_commodity_name+str(date_object)
        
        try:
            os.mkdir(loc)
        except:
            print('Folder exists.')
            
        # rawdata=pyreadr.read_r(input_file_name)[None]
        rawdata = pd.read_csv(input_file_name) 
        
        if input_commodity_name == 'MOTOR':
            attr_list=[
            'VENDOR_ID',
            'BASEPLATE_LOT_NUM',
            'BASEPLATE_MOLD_NUM',
            'BASEPLATE_CAVITY_NUM',
            'MOTOR_LINE_NUM',
            'RAMP_DATE_CODE',
            'RAMP_MOLD_NUM',
            'RAMP_CAVITY_NUM']
        
        elif input_commodity_name == 'STMCV':
            attr_list = ['STMCV_2D_BARCODE',
                     'GROUP_NUM'
                       
                    ]
            
        elif input_commodity_name == 'ECM':
            lotnum = rawdata['LOT_NUM']
            Tooling_Code = []
            
            for i in barcode:
                if len(i) >= 19:
                    Tooling_Code.append(i[17:19])
                else:
                    Tooling_Code.append('')
                    
            rawdata['Tooling_Code'] =Tooling_Code
            attr_list = ['LOT_NUM',
                         'Tooling_Code']
            
        elif input_commodity_name == 'CLAMP':
            attr_list = ['CLAMP_DATECODE','TYPE_PROCESS','MACHINE_TOOL_LINE_NO','CLAMP_LOT_TYPE','CLAMP_SAMPLE_NUM']
            
        else:
            attr_list_read = pd.read_excel(PER_UPLOAD_PATH + '/key traceability new 22-1-4.xlsx')
            
            if input_commodity_name == 'CASTEBASEPLATE':
                attr_list1=attr_list_read[attr_list_read['Commodity'] == 'CasteBaseplate']
            elif input_commodity_name == 'RAMP':
                attr_list1=attr_list_read[attr_list_read['Commodity'] == 'Ramp']
            elif input_commodity_name == 'HOOKUP':
                attr_list1=attr_list_read[attr_list_read['Commodity'] == 'Hookup']
            else:
                attr_list1=attr_list_read[attr_list_read['Commodity'] == input_commodity_name]
            
            attr_list = attr_list1['Attribute Name'].unique().tolist()

        CTQ_LIST_read = pd.read_csv('QPM_CTQ_LIST.csv')
        
        if input_commodity_name == 'MOTOR':
            CTQ_LIST1=CTQ_LIST_read[CTQ_LIST_read['COMMODITY'] == 'MO']
        else:
            CTQ_LIST1=CTQ_LIST_read[CTQ_LIST_read['COMMODITY'] == input_commodity_name]
        
        CTQ_LIST = CTQ_LIST1['QPM_PARAMETER'].unique().tolist()

        dif_sum = []
        shift_type = []
        pn = []
        Para = []
        lot_num = []
        trigger_attr = []
        Greatest_attribute_diff = []
        new_lot_diff = []
        Grestest_attr_per_cent_in_baseline = []
        Grestest_attr_per_cent_in_newlot = []
        atrr_name = []
        attr_per_cent_in_baseline = []
        attr_per_cent_in_newlot = []

        par_list=[i for i in rawdata.columns if i in CTQ_LIST]
        sum_sup=[]
        sum_parameter=[]
        sup_partnum=[]
        corr_1=[]
        corr_kbest=[]
        sup_name = []
        
        for i in range(len(CTQ_LIST)):
            parameter = CTQ_LIST[i]
            
            if not parameter in rawdata.columns:
                continue
                
            rawdata1=rawdata.dropna(subset=[parameter])
            
            if input_commodity_name == 'MOTOR':
                partn_sup_groupnum=pre_prpcessing_motor(rawdata1,parameter)
            else:
                partn_sup_groupnum=pre_prpcessing(rawdata1,parameter)
                
            for partnum in partn_sup_groupnum.keys():

                df_motor=rawdata1[rawdata1['PART_NUM']==partnum]
                if input_commodity_name == 'MOTOR':
                    partn_sup_groupnum=pre_prpcessing_motor(df_motor,parameter)
                if len(partn_sup_groupnum[partnum].keys())>0:
                    for sup in partn_sup_groupnum[partnum].keys():
                        new_groupnum=partn_sup_groupnum[partnum][sup]
                        if input_commodity_name == 'MOTOR':
                            new_str = str(new_groupnum)
                            new_groupnum = int(new_str[3]+new_str[1]+new_str[2]+new_str[0])

                        df_motor1=df_motor[df_motor['SUPPLIER_NAME']==sup]#100860536

                        df_motor1.dropna(axis=1,how='all',inplace=True)
                        ######
                        if len(df_motor1)>0:
                            con_attr=[i for i in df_motor1.columns if i in attr_list]
                            df_attr=df_motor1[con_attr+[parameter]]        

                            if input_commodity_name == 'MOTOR':
                                from sklearn.feature_selection import VarianceThreshold
                                var_thres=VarianceThreshold(threshold=0)
                                
                                try:
                                      var_thres.fit(df_attr)
                                except:
                                      print('Failed var_thres.')
                                      continue
                                    
                                df_attr.columns[var_thres.get_support()]
                                constant_columns = [column for column in df_attr.columns
                                                    
                                if column not in df_attr.columns[var_thres.get_support()]]
                                con_attr=[i for i in con_attr if i not in constant_columns]

                            if len(con_attr)==0:
                                pass
                            else:
                                df_b=df_motor[con_attr+[parameter]][df_motor['GROUP_NUM'].isin([new_groupnum])==False]
                                df_t=df_motor[con_attr+[parameter]][df_motor['GROUP_NUM'].isin([new_groupnum])]
                                df_b['status']='BASELINE'
                                df_t['status']='NEWDATA'

                                m_shift=dict()
                                v_shift=dict()
                                final=[]
                                split_con=[]
                                check_attr=[]
                                attr_detail=[]
                                
                                for j in range(len(attr_list)):
                                    attr = attr_list[j]
                                    if input_commodity_name in ['VCMA','MOTOR','CLAMP','SPACER','DSP','ECM']:
                                        if input_commodity_name == 'STMCV':
                                            readin_sheet = pd.read_excel(PER_UPLOAD_PATH+'Sheet.xlsx',sheet_name = 'COVER')
                                        else:
                                            readin_sheet = pd.read_excel(PER_UPLOAD_PATH+'Sheet.xlsx',sheet_name = input_commodity_name)
                                        try:
                                            k = readin_sheet[attr][readin_sheet[readin_sheet.keys().tolist()[0]].tolist().index(parameter)]
                                        except:
                                            continue
                                        if not(k == 'yes' or k == 'Yes' or k == 'YES'):
                                            continue
                                            
                                        # if not(k.upper() == 'YES'):
                                            # continue
                                            
                                    print(attr+parameter)
                                    if not attr in df_t.columns:
                                        continue
                                    try:
                                        c_m,c_v=lvl_two_shift(parameter,attr,df_b,df_t)
                                        m_shift.update({attr:c_m})
                                        v_shift.update({attr:c_v})
                                    except:
                                        split_con.append(sup+'_'+str(partnum)+'_'+parameter)
                                        check_attr.append(attr)
                                        attr_detail.append('attribute changed,{0} has {1}, but not included in the baseline'.format(attr,str(df_t[attr].unique())))

                                if len(m_shift)>0:
                                    final_result=gen_result(m_shift,v_shift)
                                    final_result['mean_thresold_ratio']=(final_result['Mean Shift']-final_result['Mean Threshold'])/final_result['Mean Threshold']
                                    final_result['variance_thresold_ratio']=(final_result['Variance Shift']-final_result['Variance Threshold'])/final_result['Variance Threshold']
                                    title=sup+'('+str(partnum)+')_'+parameter+'_('+str(new_groupnum)+')'
                                    if final_result[final_result['Mean Shift']-final_result['Mean Threshold']>0].shape[0]>0:
                                        
                                        try:
                                            dif_sum = plot1(input_commodity_name,loc,df_motor,parameter,partnum,new_groupnum,final_result,title,'Mean Shift','Mean Threshold')
                                        except Exception as e:
                                            print('Failed plot1: '+ e)
                                            continue
                                        shift_type.append('mean shift')
                                        pn.append(partnum)
                                        Para.append(dif_sum[0])
                                        lot_num.append(new_groupnum)
                                        trigger_attr.append(dif_sum[1])
                                        Greatest_attribute_diff.append(dif_sum[2])
                                        nld = abs(float(dif_sum[3]))
                                        new_lot_diff.append(nld)
                                        Grestest_attr_per_cent_in_baseline.append(dif_sum[4])
                                        Grestest_attr_per_cent_in_newlot.append(dif_sum[5])
                                        atrr_name.append(dif_sum[6])
                                        attr_per_cent_in_baseline.append(dif_sum[7])
                                        attr_per_cent_in_newlot.append(dif_sum[8])
                                        sup_name.append(dif_sum[9])
                                    if final_result[final_result['Variance Shift']-final_result['Variance Threshold']>0].shape[0]>0:                                    
                                        try:
                                            dif_sum = plot1(input_commodity_name,loc,df_motor,parameter,partnum,new_groupnum,final_result,title,'Variance Shift','Variance Threshold')
                                        except Exception as e:
                                            print('Failed plot1: '+ e)
                                            continue
                                        shift_type.append('variance shift')
                                        pn.append(partnum)
                                        Para.append(dif_sum[0])
                                        lot_num.append(new_groupnum)
                                        trigger_attr.append(dif_sum[1])
                                        Greatest_attribute_diff.append(dif_sum[2])
                                        nld = abs(float(dif_sum[3]))
                                        new_lot_diff.append(nld)
                                        Grestest_attr_per_cent_in_baseline.append(dif_sum[4])
                                        Grestest_attr_per_cent_in_newlot.append(dif_sum[5])
                                        atrr_name.append(dif_sum[6])
                                        attr_per_cent_in_baseline.append(dif_sum[7])
                                        attr_per_cent_in_newlot.append(dif_sum[8])
                                        sup_name.append(dif_sum[9])
                                    ##print ('{0}_{1}_{2} config anaysis already done'.format(parameter,partnum,new_groupnum))

                                    ###*********


        graph = {'supplier name':sup_name,
            'shift type':shift_type,
            'pn':pn ,
            'parameter':Para,
            'lot#':lot_num,
            'trigger attr':trigger_attr,
            'Greatest attribute diff':Greatest_attribute_diff,
            'new lot diff':new_lot_diff,
            'Grestest attr% in baseline':Grestest_attr_per_cent_in_newlot,
            'Grestest attr% in newlot':Grestest_attr_per_cent_in_baseline,
            'atrr name':atrr_name,
            'attr% in baseline':attr_per_cent_in_baseline,
            'attr% in newlot':attr_per_cent_in_newlot }
            
        excel_file_name = str(input_commodity_name) + '_diff_summary'+str(date_object)+'.xlsx'
        pd.DataFrame(graph).to_excel(loc+'/'+excel_file_name,index=False)
        
        
        folder_list = os.listdir('./')

        df = pd.DataFrame()
        for folder_name in folder_list:    
            if input_commodity_name in folder_name:

                try:
                    file_list = os.listdir('./'+folder_name) 
                except:            
                    continue

                for file_name in file_list:
                    if file_name.endswith('.xlsx') or file_name.endswith('.xls'):

                        data = pd.read_excel(OUTPUT_PATH+folder_name+'/'+file_name)
                        if file_name.endswith('.xlsx'):
                            file_date = file_name[(len(input_commodity_name)+len('_diff_summary')):-5]
                        else:
                            file_date = file_name[(len(input_commodity_name)+len('_diff_summary')):-4]
                        
                        data['Date'] = [file_date] * len(data)
                        df = df.append(data, ignore_index=True) 
        pd.DataFrame(df).to_excel(OUTPUT_PATH+input_commodity_name+'_all_summary.xlsx',index=False)
