import pandas as pd
import plotly.express as px
import plotly.graph_objects as go

TOTAL = 1000
TIMEOUT = 20000

lida = pd.read_csv('experiment/results-lida.csv')
lida = lida.sort_values(["time"]).reset_index(drop=True)
lida = lida[lida['result'] != "other"]

z3seq = pd.read_csv('experiment/results-z3seq.csv')
z3seq = z3seq.sort_values(["time"]).reset_index(drop=True)
z3seq = z3seq[z3seq['result'] != "other"]

z3arr = pd.read_csv('experiment/results-z3arr.csv')
z3arr = z3arr.sort_values(["time"]).reset_index(drop=True)
z3arr = z3arr[z3arr['result'] != "other"]

z3las = pd.read_csv('experiment/results-z3las.csv')
z3las = z3las.sort_values(["time"]).reset_index(drop=True)
z3las = z3las[z3las['result'] != "other"]

z3str4 = pd.read_csv('experiment/results-z3str4.csv')
z3str4 = z3str4.sort_values(["time"]).reset_index(drop=True)
z3str4 = z3str4[z3str4['result'] != "other"]

# fig = px.scatter(x = lida.index, y = lida['time'], title='Cactus Plot of Lida')
fig = go.Figure()
fig.update_layout(
    # title="Cactus Plot of Lida, the Component Algorithms (Z3Seq, Z3Arr, and Z3Las), and Z3str4",
    xaxis_title="Number Solved",
    yaxis_title="Time (ms)",
    legend_title="Solver",
    font=dict(
        size=24
    )
)

fig.add_scatter(y = z3str4['time'], mode='markers', name="Z3str4")
fig.add_scatter(y = lida['time'], mode='markers', name="Lida")
fig.add_scatter(y = z3seq['time'], mode='markers', name="Z3Seq")
fig.add_scatter(y = z3arr['time'], mode='markers', name="Z3Arr")
fig.add_scatter(y = z3las['time'], mode='markers', name="Z3LAS")

fig.update_traces(marker=dict(size=20,line=dict(width=2)))

fig.show()

print("solver,solved,time,par-2")
print(",".join(["lida", str(len(lida)), str(lida['time'].sum()), str(lida['time'].sum() + (TOTAL - len(lida))*TIMEOUT)]))
print(",".join(["z3seq", str(len(z3seq)), str(z3seq['time'].sum()), str(z3seq['time'].sum() + (TOTAL - len(z3seq))*TIMEOUT)]))
print(",".join(["z3arr", str(len(z3arr)), str(z3arr['time'].sum()), str(z3arr['time'].sum() + (TOTAL - len(z3arr))*TIMEOUT)]))
print(",".join(["z3las", str(len(z3las)), str(z3las['time'].sum()), str(z3las['time'].sum() + (TOTAL - len(z3las))*TIMEOUT)]))
print(",".join(["z3str4", str(len(z3str4)), str(z3str4['time'].sum()), str(z3str4['time'].sum() + (TOTAL - len(z3str4))*TIMEOUT)]))