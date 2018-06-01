import pandas as pd
from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot
from plotly.graph_objs import *

data = pd.read_excel("a.xlsx")
trace1 = Scatter(
    x=data['DATE'],
    y=(1+data['RTN.x']).cumprod(),
    name='return'
)
trace2 = Scatter(
    x=data['DATE'],
    y=(1+data['RTN.y']).cumprod(),
    name='benchmark'
)
trace3 = Scatter(
    x=data['DATE'],
    y=(1+data['RTN.x']).cumprod() - (1+data['RTN.y']).cumprod(),
    name='res_rtn',
    yaxis='y2'
)
data = [trace1, trace2, trace3]
layout = Layout(
    title='smb momentum',
    yaxis=dict(
        title=''
    ),
    yaxis2=dict(
        title='',
        titlefont=dict(
            color='rgb(148, 103, 189)'
        ),
        tickfont=dict(
            color='rgb(148, 103, 189)'
        ),
        overlaying='y',
        side='right'
    )
)
fig = Figure(data=data, layout=layout)
plot(fig)
