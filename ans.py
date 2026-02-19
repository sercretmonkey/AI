from sympy import symbols, And, Or, Not, Implies

# Q1: Loan Evaluation System — Boolean Logic with sympy
print("=" * 50)
print("Q1: LOAN EVALUATION SYSTEM")
print("=" * 50)

credit_score, income, employment, dti, collateral, repayment = symbols(
    'credit_score income employment dti collateral repayment')

loan_approval = And(credit_score, income, employment, Not(dti), collateral, repayment)

criteria_labels = {
    credit_score: "Credit Score < 650",
    income: "Income < 8,00,000",
    employment: "Employment < 3 years",
    dti: "Debt-to-Income > 40%",
    collateral: "Collateral < 10% of loan",
    repayment: "Recent defaults",
}

def evaluate_loan(data, name):
    print(f"\n{name}'s Application:")
    result = loan_approval.subs(data)
    print(f"Decision: {'APPROVED' if result else 'REJECTED'}")
    if not result:
        for var, reason in criteria_labels.items():
            if var == dti and data.get(var): print(f"  Failed: {reason}")
            elif var != dti and not data.get(var): print(f"  Failed: {reason}")

evaluate_loan({credit_score: True, income: True, employment: True,
               dti: False, collateral: True, repayment: True}, "Ramesh")

evaluate_loan({credit_score: False, income: True, employment: False,
               dti: True, collateral: True, repayment: True}, "Priya")


# Q2: Security System — Modus Ponens, Modus Tollens, Syllogism
print("\n" + "=" * 50)
print("Q2: SECURITY SYSTEM INFERENCE")
print("=" * 50)

def security_inference(has_malfunction, has_investigation, case):
    print(f"\n{case}: malfunction={has_malfunction}, investigation={has_investigation}")
    has_notify = has_investigation
    if not has_investigation:
        print("Modus Tollens: No investigation => No notification")
    has_intruder = has_notify
    if not has_notify:
        print("Modus Tollens: No notification => No intruder")
    has_alarm = has_intruder or has_malfunction
    if not has_alarm:
        print("Modus Tollens: No intruder + No malfunction => Alarm NOT triggered")
    else:
        print("Modus Ponens: Alarm WAS triggered")
    print(f"Result: Intruder={has_intruder}, Alarm={has_alarm}")

security_inference(False, False, "Case 1")
security_inference(False, True, "Case 2")


# Q3: Traffic Management System — Boolean Logic for traffic decisions
print("\n" + "=" * 50)
print("Q3: TRAFFIC MANAGEMENT SYSTEM")
print("=" * 50)

rush_hour, heavy_traffic, emergency_vehicle = symbols('rush_hour heavy_traffic emergency_vehicle')
bad_weather, special_event, road_closed, high_pedestrian = symbols(
    'bad_weather special_event road_closed high_pedestrian')

traffic_adjustment = Or(emergency_vehicle, And(rush_hour, heavy_traffic),
                        bad_weather, special_event, road_closed, high_pedestrian)

def evaluate_traffic(data, case):
    print(f"\n{case}:")
    green = 30
    reasons = []
    if data.get(emergency_vehicle): green, _ = 60, reasons.append("Emergency vehicle — max green")
    if data.get(rush_hour) and data.get(heavy_traffic): green, _ = max(green, 50), reasons.append("Rush hour congestion — extended green")
    if data.get(bad_weather): reasons.append("Bad weather — bus priority")
    if data.get(special_event): green, _ = max(green, 45), reasons.append("Special event — adjusted")
    if data.get(road_closed): reasons.append("Road closed — rerouted")
    if data.get(high_pedestrian): green, _ = max(green, 40), reasons.append("High pedestrian — extended crossing")
    if reasons:
        print(f"Green light: {green}s")
        for r in reasons: print(f"  - {r}")
    else:
        print(f"Standard flow. Green light: {green}s")

evaluate_traffic({rush_hour: True, heavy_traffic: True, emergency_vehicle: True,
                  bad_weather: True, special_event: False, road_closed: False,
                  high_pedestrian: False}, "Scenario 1: Rush Hour Emergency")

evaluate_traffic({rush_hour: False, heavy_traffic: False, emergency_vehicle: False,
                  bad_weather: False, special_event: False, road_closed: False,
                  high_pedestrian: False}, "Scenario 2: Normal Conditions")


# Q4: Legal Reasoning — Forward and Backward Chaining
print("\n" + "=" * 50)
print("Q4: LEGAL REASONING SYSTEM")
print("=" * 50)

def forward_chain(ev, case):
    print(f"\n{case} (Forward Chaining):")
    print(f"Evidence: E1={ev['E1']}, E2={ev['E2']}, E3={ev['E3']}")
    M = ev['E1'] and ev['E2']
    I = M and ev['E3']
    G = I
    print(f"E1+E2 => M={M}, M+E3 => I={I}, I => G={G}")
    print(f"Verdict: {'GUILTY' if G else 'NOT GUILTY'}")

def backward_chain(ev, case):
    print(f"\n{case} (Backward Chaining):")
    print("Goal: G? => Need I => Need M+E3 => Need E1+E2")
    M = ev['E1'] and ev['E2']
    I = M and ev['E3']
    G = I
    print(f"E1={ev['E1']}, E2={ev['E2']} => M={M}")
    print(f"M={M}, E3={ev['E3']} => I={I} => G={G}")
    print(f"Verdict: {'GUILTY' if G else 'NOT GUILTY'}")

forward_chain({'E1': True, 'E2': True, 'E3': True}, "Case 1")
backward_chain({'E1': True, 'E2': True, 'E3': True}, "Case 1")

forward_chain({'E1': True, 'E2': False, 'E3': True}, "Case 2")
backward_chain({'E1': True, 'E2': False, 'E3': True}, "Case 2")


# Q5: Access Permission System — Boolean Logic for security access
print("\n" + "=" * 50)
print("Q5: ACCESS PERMISSION SYSTEM")
print("=" * 50)

clearance, schedule, access_request, biometric = symbols(
    'clearance schedule access_request biometric')

access_granted = And(clearance, schedule, access_request, biometric)

fail_labels = {clearance: "Clearance below Level 3", schedule: "Outside work hours",
               access_request: "No approved request", biometric: "Biometric failed"}

def evaluate_access(data, name):
    print(f"\n{name}:")
    result = access_granted.subs(data)
    print(f"Decision: {'GRANTED' if result else 'DENIED'}")
    if not result:
        for var, reason in fail_labels.items():
            if not data.get(var): print(f"  Failed: {reason}")

evaluate_access({clearance: True, schedule: True,
                 access_request: True, biometric: True}, "Employee A")
evaluate_access({clearance: True, schedule: True,
                 access_request: False, biometric: False}, "Employee B")


# Q6: Healthcare Resource Allocation — AI-based dynamic resource management
print("\n" + "=" * 50)
print("Q6: HEALTHCARE RESOURCE ALLOCATION")
print("=" * 50)

critical_patient, emergency, pandemic = symbols('critical_patient emergency pandemic')
high_volume, staff_available, beds_available = symbols('high_volume staff_available beds_available')

needs_action = Or(critical_patient, emergency, And(pandemic, Not(beds_available)),
                  And(high_volume, Not(staff_available)))

def evaluate_hospital(data, case):
    print(f"\n{case}:")
    actions = []
    if data.get(critical_patient): actions.append("Allocate ICU + senior doctor")
    if data.get(emergency): actions.append("Activate emergency protocol")
    if data.get(pandemic) and not data.get(beds_available): actions.append("Setup overflow ward")
    if data.get(high_volume) and not data.get(staff_available): actions.append("Redistribute staff, delay non-urgent")
    if actions:
        for a in actions: print(f"  - {a}")
    else:
        print("  Normal operations")

evaluate_hospital({critical_patient: True, emergency: True, pandemic: True,
                   high_volume: True, staff_available: False, beds_available: False},
                  "Scenario 1: Pandemic Crisis")

evaluate_hospital({critical_patient: False, emergency: False, pandemic: False,
                   high_volume: False, staff_available: True, beds_available: True},
                  "Scenario 2: Normal Day")
