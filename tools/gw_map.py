#!/usr/bin/env python3
"""gw_map.py - curated mapping of BillingCore / ClaimsCore Db2 columns to a
default Guidewire BillingCenter / ClaimCenter data model.

Confidence:  DM  = base data-model entity.field (verify in the target
                   release's Data Dictionary)
             IND = indicative: entity is right, field name is a best fit to
                   be confirmed
             EXT = proposed customer extension (_Ext) or product-model item
             --  = no Guidewire target (platform-managed or retired)
Class:  direct · transform · extension · derived · gap · retire
Columns not listed fall to the rules in classify(); every result says
which rule produced it.
"""
import re

# (entity, app, note) per table, then column -> (field, class, conf, rule)
T = {}
def t(table, app, entity, note, cols):
    T[table] = {'app': app, 'entity': entity, 'note': note, 'cols': cols}

D, X, E, V, G, R = 'direct', 'transform', 'extension', 'derived', 'gap', 'retire'

# ======================================================= BillingCenter
t('BIL_CONTROL', 'BC', 'Platform (batch processing / system clock)',
  'Business date and batch state are managed by the BillingCenter server and batch-process framework.', {
  'BUSINESS_DATE': ('System clock / test-time configuration', R, '--', 'Not migrated; GW uses server date (with test clock in lower environments)'),
  'BATCH_STATUS': ('ProcessHistory (batch process status)', R, '--', 'Replaced by batch process monitoring'),
  'CURRENCY_CODE': ('Currency typelist / default currency config', X, 'DM', 'Single currency GBP configured as default currency'),
})
t('BIL_SEQUENCE', 'BC', 'Platform (numbering plugins)', 'Key and number generation is handled by GW ID generation and numbering plugins.', {
  'SEQ_NAME': ('IAccountNumberPlugin / invoice numbering plugin', X, 'IND', 'Business-number sequences (ACTNO, INVNO, CHQNO) become numbering plugin configuration; surrogate sequences retire'),
})
t('BIL_CODE_TABLE', 'BC', 'Typelists', 'Each TABLE_NAME becomes a typelist; codes become typecodes.', {
  'TABLE_NAME': ('Typelist name', X, 'DM', 'One typelist (or typelist extension) per table name'),
  'CODE_VALUE': ('Typecode code', X, 'DM', 'Typecodes are case-sensitive; lower-case on migration'),
  'DESCRIPTION': ('Typecode name / description', D, 'DM', ''),
  'SORT_ORDER': ('Typecode priority', D, 'DM', ''),
  'ACTIVE_FLAG': ('Typecode retired flag', X, 'DM', 'Y → retired=false'),
})
t('BIL_CALENDAR', 'BC', 'Holiday', 'Business calendar holidays used for due-date rolling.', {
  'CAL_DATE': ('Holiday.OccurrenceDate', D, 'DM', ''), 'HOLIDAY_NAME': ('Holiday.Name', D, 'DM', ''),
  'HOLIDAY_FLAG': ('(row exists only for holidays)', X, '--', 'Only rows with HOLIDAY_FLAG=Y migrate'),
  'DAY_OF_WEEK': ('derived', V, '--', 'Weekends are configured, not stored'),
})
t('BIL_PAYMENT_PLAN', 'BC', 'PaymentPlan', 'Payment plans are admin data in BillingCenter.', {
  'PLAN_CODE': ('PaymentPlan.Name (+ PublicID)', X, 'DM', 'Legacy code kept as PublicID suffix'),
  'PLAN_NAME': ('PaymentPlan.Description', D, 'IND', ''),
  'DOWNPAY_PCT': ('PaymentPlan.DownPaymentPercent', D, 'DM', ''),
  'NUM_INSTALLMENTS': ('PaymentPlan.MaximumNumberOfInstallments', X, 'DM', 'Legacy count includes the deposit; GW counts installments after the down payment → subtract 1'),
  'FREQUENCY': ('PaymentPlan.Periodicity', X, 'DM', 'Value map M→monthly, Q→quarterly, S→everysixmonths, A→everyyear'),
  'INSTALL_FEE': ('PaymentPlan installment fee', X, 'IND', 'Installment fee amount on the plan'),
  'DAYS_BILL_BEFORE': ('PaymentPlan invoicing lead time (days before due)', X, 'IND', ''),
  'FIRST_INST_MONTHS': ('PaymentPlan first-installment offset', X, 'IND', ''),
  'MIN_INST_AMOUNT': ('MinimumInstallmentAmount_Ext', E, 'EXT', 'Small-installment merge rule (BR-BIL-SCH) has no base field'),
  'ACTIVE_FLAG': ('PaymentPlan.ExpirationDate', X, 'IND', 'Inactive → set expiration date'),
})
t('BIL_DELINQ_PLAN', 'BC', 'DelinquencyPlan', 'Delinquency plan with grace period, fees and thresholds.', {
  'PLAN_CODE': ('DelinquencyPlan.Name', X, 'DM', ''), 'PLAN_NAME': ('DelinquencyPlan.Description', D, 'IND', ''),
  'GRACE_DAYS': ('DelinquencyPlan.GracePeriodDays', D, 'DM', ''),
  'LATE_FEE': ('DelinquencyPlan.LateFeeAmount', X, 'DM', 'MonetaryAmount GBP'),
  'REINSTATE_FEE': ('DelinquencyPlan.ReinstatementFeeAmount', X, 'DM', 'MonetaryAmount GBP'),
  'PASTDUE_THRESHOLD': ('DelinquencyPlan.AcctEnterDelinquencyThreshold', X, 'DM', ''),
  'WRITEOFF_THRESHOLD': ('DelinquencyPlan.WriteoffThreshold', X, 'DM', ''),
})
t('BIL_DELINQ_STEP', 'BC', 'DelinquencyPlanWorkflow → DelinquencyPlanEvent', 'Timed steps become delinquency workflow events.', {
  'EVENT_CODE': ('DelinquencyPlanEvent.EventName', X, 'DM', 'REMIND→DunningLetter1, LATEFEE→LateFee, NOI→NoticeOfIntent, CANCREQ→Cancellation, WRITEOFF→WriteOff'),
  'DAYS_AFTER_START': ('DelinquencyPlanEvent.OffsetDays', D, 'DM', ''),
  'STEP_SEQ': ('DelinquencyPlanEvent order', V, 'DM', ''), 'DOC_TYPE': ('Document template for the event', X, 'IND', ''),
})
t('BIL_COMMISSION_PLAN', 'BC', 'CommissionPlan', 'Producer commission plans.', {
  'PLAN_CODE': ('CommissionPlan.Name', X, 'DM', ''), 'PLAN_NAME': ('CommissionPlan.Description', D, 'IND', ''),
  'DEFAULT_RATE_BPS': ('CommissionSubPlan.CommissionRate (default)', X, 'DM', 'Basis points → percent (÷100)'),
  'EARN_BASIS': ('CommissionSubPlan.PayableCriteria', X, 'DM', 'P (on payment) → onpayment, B → onbilling'),
  'PAY_THRESHOLD': ('CommissionPlan payment threshold', X, 'IND', ''),
})
t('BIL_COMMISSION_RATE', 'BC', 'CommissionSubPlan / CommissionSubPlanChargePatternRate', 'Rates by product and charge pattern.', {
  'PRODUCT_CODE': ('CommissionSubPlan LOB restriction', X, 'IND', 'M→PersonalAuto, H→Homeowners, C→CommercialProperty'),
  'CHARGE_PATTERN': ('ChargePattern (rate override)', X, 'DM', ''), 'RATE_BPS': ('CommissionRate', X, 'DM', 'bps → percent'),
})
t('BIL_AUTHORITY_LIMIT', 'BC', 'AuthorityLimitProfile → AuthorityLimit', 'Role-based monetary authority.', {
  'USER_ROLE': ('AuthorityLimitProfile', X, 'DM', 'One profile per legacy role'),
  'LIMIT_TYPE': ('AuthorityLimit.LimitType', X, 'DM', 'WRITEOFF→writeoff, DISBURSE→disbursement, REVERSAL→payment reversal'),
  'LIMIT_AMOUNT': ('AuthorityLimit.LimitAmount', D, 'DM', ''),
})
t('BIL_USER', 'BC', 'User / Credential / UserRole', 'Users and supervisors.', {
  'USER_ID': ('Credential.UserName', D, 'DM', 'Identity federated through the IdP'),
  'USER_NAME': ('User.Contact (UserContact) name', X, 'DM', ''),
  'USER_ROLE': ('UserRole + AuthorityProfile', X, 'DM', ''),
  'SUPERVISOR_ID': ('Group.Supervisor', X, 'DM', 'Supervisor chain becomes group hierarchy'),
})
t('BIL_ACCOUNT', 'BC', 'Account', 'Billing account — the central BillingCenter entity.', {
  'ACCOUNT_NUMBER': ('Account.AccountNumber', D, 'DM', 'Keep legacy number (AC + 8 digits)'),
  'GENAPP_CUST_NUM': ('Account ↔ PolicyCenter account link', X, 'DM', 'Resolved to the PolicyCenter AccountNumber migrated from GenApp customer'),
  'ACCOUNT_NAME': ('Account.AccountName', D, 'DM', ''),
  'ACCOUNT_TYPE': ('Account.AccountType', X, 'DM', 'PERS→personal, COMM→commercial'),
  'BILLING_LEVEL': ('Account.BillingLevel', X, 'DM', 'ACCT→account, PLCY→policy (default invoice streams per policy)'),
  'INVOICE_DAY': ('Account.InvoiceDayOfMonth', D, 'DM', ''),
  'DELIVERY_METHOD': ('Account.InvoiceDeliveryType', X, 'DM', 'MAIL→mail, EMAL→email, BOTH→mailandemail'),
  'DELINQ_PLAN_CODE': ('Account.DelinquencyPlan', X, 'DM', 'Reference by plan name'),
  'AUTOPAY_FLAG': ('Account.DefaultPaymentInstrument (Responsive)', X, 'IND', 'Autopay = default instrument of type ACH/card'),
  'AUTOPAY_INSTR_ID': ('Account.DefaultPaymentInstrument', X, 'DM', ''),
  'ACCOUNT_STATUS': ('Account.CloseDate / status', X, 'IND', 'CLSD → close date set'),
  'COLLECTION_STATUS': ('Account.DelinquencyStatus (derived)', V, 'DM', 'Derived from open DelinquencyProcess'),
  'UNAPPLIED_AMT': ('Account.UnappliedAmount (UnappliedFund)', V, 'DM', 'Computed from unapplied funds; load via opening-balance conversion'),
  'BILLED_AMT': ('Account billed balance (T-account)', V, 'DM', 'Derived from invoice items; never loaded directly'),
  'DUE_AMT': ('Account due balance (T-account)', V, 'DM', 'Derived'),
  'UNBILLED_AMT': ('Account unbilled balance (T-account)', V, 'DM', 'Derived'),
  'CURRENCY_CODE': ('Account.Currency', D, 'DM', ''),
  'OPEN_DATE': ('Account.CreationDate', D, 'IND', ''), 'CLOSE_DATE': ('Account.CloseDate', D, 'DM', ''),
})
t('BIL_ACCOUNT_CONTACT', 'BC', 'AccountContact + Contact (Person/Company) + Address', 'Payer and other account contacts.', {
  'CONTACT_ROLE': ('AccountContactRole', X, 'DM', 'PAYR→payer (primarypayer when PRIMARY_FLAG)'),
  'CONTACT_TYPE': ('Contact subtype', X, 'DM', 'P→Person, C→Company'),
  'FIRST_NAME': ('Person.FirstName', D, 'DM', ''), 'LAST_NAME': ('Person.LastName', D, 'DM', ''),
  'COMPANY_NAME': ('Company.Name', D, 'DM', ''),
  'ADDRESS_LINE1': ('Address.AddressLine1', D, 'DM', ''), 'ADDRESS_LINE2': ('Address.AddressLine2', D, 'DM', ''),
  'CITY': ('Address.City', D, 'DM', ''), 'POSTCODE': ('Address.PostalCode', D, 'DM', 'Already normalised by BILACCT0'),
  'COUNTRY_CODE': ('Address.Country', D, 'DM', ''), 'EMAIL_ADDRESS': ('Contact.EmailAddress1', D, 'DM', ''),
  'PHONE_NUMBER': ('Contact.WorkPhone / HomePhone', X, 'DM', 'Route by number range'),
  'PRIMARY_FLAG': ('AccountContact.PrimaryPayer', X, 'DM', ''), 'CONTACT_SEQ': ('(ordering)', V, '--', ''),
})
t('BIL_PAYMENT_INSTRUMENT', 'BC', 'PaymentInstrument', 'Tokenised instruments (no raw card/bank numbers held).', {
  'INSTR_TYPE': ('PaymentInstrument.PaymentMethod', X, 'DM', 'ACH→ach, CARD→creditcard, CHEQ→check, CASH→cash'),
  'TOKEN_REF': ('PaymentInstrument.Token', D, 'DM', 'Vault token passes through unchanged'),
  'LAST_FOUR': ('PaymentInstrument.Detail (masked)', X, 'IND', ''),
  'EXPIRY_MMYY': ('PaymentInstrument card expiry', X, 'IND', ''),
  'SORT_CODE': ('PaymentInstrument bank routing (sort code)', X, 'IND', 'UK sort code into routing number field'),
  'HOLDER_NAME': ('PaymentInstrument account holder', X, 'IND', ''),
  'INSTR_STATUS': ('PaymentInstrument.Immutable / retired', X, 'IND', ''),
  'FAIL_COUNT': ('FailureCount_Ext', E, 'EXT', '3-strike deactivation rule (BR-BIL-PIN)'),
})
t('BIL_HOLD', 'BC', 'Hold → HoldType', 'Account-level holds.', {
  'HOLD_TYPE': ('Hold.HoldTypes', X, 'DM', 'INVC→invoicesending, DLNQ→delinquency, DISB→disbursements, PYMT→paymentdistribution'),
  'START_DATE': ('Hold.StartDate', D, 'IND', ''), 'END_DATE': ('Hold.ReleaseDate', D, 'IND', ''),
  'HOLD_REASON': ('Hold.Description', D, 'IND', ''), 'HOLD_STATUS': ('Hold release (derived)', V, 'IND', ''),
})
t('BIL_INVOICE_STREAM', 'BC', 'InvoiceStream', 'Invoice streams and anchor days.', {
  'PERIODICITY': ('InvoiceStream.Periodicity', X, 'DM', ''), 'ANCHOR_DAY': ('InvoiceStream.AnchorDate (day of month)', X, 'DM', ''),
  'STREAM_STATUS': ('InvoiceStream active (derived)', V, 'IND', ''),
})
t('BIL_POLICY_PERIOD', 'BC', 'PolicyPeriod (BillingCenter)', 'Billing view of a policy term; created from PolicyCenter billing instructions.', {
  'GENAPP_POL_NUM': ('PolicyPeriod.PolicyNumber', X, 'DM', 'Must equal the PolicyCenter policy number migrated from GenApp'),
  'TERM_NUMBER': ('PolicyPeriod.TermNumber', D, 'DM', ''),
  'PRODUCT_CODE': ('PolicyPeriod.Policy.LOBCode', X, 'DM', 'M→PersonalAuto, H→Homeowners, C→CommercialProperty'),
  'EFF_DATE': ('PolicyPeriod.PolicyPerEffDate', D, 'DM', ''), 'EXP_DATE': ('PolicyPeriod.PolicyPerExpirDate', D, 'DM', ''),
  'PAYMENT_PLAN_CODE': ('PolicyPeriod.PaymentPlan', X, 'DM', ''),
  'BILLING_METHOD': ('PolicyPeriod.BillingMethod', X, 'DM', 'DB→directbill, AB→agencybill'),
  'PRODUCER_ID': ('PolicyPeriod.PrimaryProducerCode', X, 'DM', ''),
  'TOTAL_PREMIUM': ('PolicyPeriod total charges (derived)', V, 'DM', 'Derived from charges'),
  'PP_STATUS': ('PolicyPeriod.CancelStatus / closure', X, 'IND', 'INFC→open, CANC→canceled'),
  'CANCEL_DATE': ('Cancellation billing instruction effective date', X, 'DM', ''),
  'CANCEL_REASON': ('Cancellation.CancellationReason', X, 'IND', ''),
  'CLOSURE_STATUS': ('PolicyPeriod.ClosureStatus', D, 'DM', ''),
})
t('BIL_CHARGE', 'BC', 'Charge (from BillingInstruction)', 'Charges created by billing instructions.', {
  'CHARGE_PATTERN': ('Charge.ChargePattern', X, 'DM', 'PREMIUM→Premium, TAX→Taxes, INSTFEE→InstallmentFee, LATEFEE→LateFee, REINSFEE→ReinstatementFee'),
  'BILLING_INSTR': ('BillingInstruction subtype', X, 'DM', 'NEWBIZ→NewPlcyPeriodBI, RENEWAL→Renewal, POLCHG→PolicyChange, CANCEL→Cancellation, REINST→Reinstatement'),
  'CHARGE_AMOUNT': ('Charge.Amount', D, 'DM', ''), 'CHARGE_DATE': ('Charge.ChargeDate', D, 'DM', ''),
  'EFF_DATE': ('BillingInstruction.ModificationDate', D, 'IND', ''),
  'CHARGE_STATUS': ('Charge (reversed flag)', X, 'IND', ''), 'SOURCE_REF': ('BillingInstruction external reference', X, 'IND', ''),
})
t('BIL_INVOICE_ITEM', 'BC', 'InvoiceItem', 'Scheduled pieces of a charge.', {
  'ITEM_TYPE': ('InvoiceItem.Type', X, 'DM', 'DPST→deposit, INST→installment, ONET→onetime, CRED→onetime (negative)'),
  'INSTALL_NUMBER': ('InvoiceItem.InstallmentNumber', D, 'DM', ''),
  'EVENT_DATE': ('InvoiceItem.EventDate', D, 'DM', ''), 'DUE_DATE': ('Invoice.DueDate (via invoice)', V, 'DM', ''),
  'ITEM_AMOUNT': ('InvoiceItem.Amount', D, 'DM', ''), 'PAID_AMOUNT': ('InvoiceItem.PaidAmount (derived)', V, 'DM', 'Derived from distribution items'),
  'WRITEOFF_AMOUNT': ('InvoiceItem.WrittenOffAmount (derived)', V, 'DM', ''),
  'COMMISSION_AMOUNT': ('ItemCommission.CommissionAmount', X, 'DM', ''),
  'ITEM_STATUS': ('InvoiceItem status (derived)', V, 'DM', 'Derived from invoice status and payments'),
  'PRIORITY_RANK': ('Distribution priority by ChargePattern', X, 'IND', 'Becomes payment-allocation-plan ordering'),
})
t('BIL_INVOICE', 'BC', 'AccountInvoice', 'Invoices.', {
  'INVOICE_NUMBER': ('Invoice.InvoiceNumber', D, 'DM', ''), 'BILL_DATE': ('Invoice.EventDate (bill date)', D, 'DM', ''),
  'DUE_DATE': ('Invoice.PaymentDueDate', D, 'DM', ''), 'INVOICE_AMOUNT': ('Invoice.Amount', V, 'DM', 'Derived from items'),
  'AMOUNT_DUE': ('Invoice.AmountDue', V, 'DM', ''), 'PRIOR_BALANCE': ('Invoice statement prior balance', V, 'IND', ''),
  'INVOICE_STATUS': ('Invoice.Status', X, 'DM', 'PLAN→planned, BILD→billed, DUE→due, PAID→(due, settled)'),
  'ADHOC_FLAG': ('Invoice.AdHoc', D, 'DM', ''), 'ITEM_COUNT': ('derived', V, '--', ''),
  'DOC_ID': ('Document link', X, 'IND', ''),
})
t('BIL_DOCUMENT_REQUEST', 'BC', 'Document', 'Outbound documents (invoice, dunning, NOI).', {
  'DOC_TYPE': ('Document.Type / template', X, 'DM', ''), 'DELIVERY_METHOD': ('Document delivery', X, 'IND', ''),
  'DOC_STATUS': ('Document.Status', X, 'DM', ''), 'REQUEST_DATE': ('Document.DateCreated', D, 'IND', ''),
  'SENT_TS': ('Document delivery timestamp', D, 'IND', ''), 'REF_TYPE': ('Document owner link', V, 'IND', ''), 'REF_ID': ('Document owner link', V, 'IND', ''),
})
t('BIL_PAYMENT_RECEIPT', 'BC', 'DirectBillMoneyRcvd', 'Money received.', {
  'RECEIVED_DATE': ('PaymentMoneyReceived.ReceivedDate', D, 'DM', ''), 'RECEIPT_AMOUNT': ('PaymentMoneyReceived.Amount', D, 'DM', ''),
  'UNAPPLIED_AMOUNT': ('UnappliedFund balance (derived)', V, 'DM', ''),
  'PAYMENT_METHOD': ('PaymentInstrument.PaymentMethod', X, 'DM', ''), 'REF_NUMBER': ('PaymentMoneyReceived.RefNumber', D, 'DM', ''),
  'INVOICE_NUMBER': ('DirectBillMoneyRcvd target invoice', X, 'IND', ''),
  'RECEIPT_SOURCE': ('PaymentMoneyReceived source (lockbox/online/CSR)', X, 'IND', ''),
  'RECEIPT_STATUS': ('PaymentMoneyReceived reversal status', X, 'IND', 'RVSD → reversed'),
  'REVERSAL_REASON': ('PaymentReversalReason', X, 'DM', 'NSF→nsf, WRONGACC→wrongaccount, REFUND, STOPPYMT'),
  'BATCH_REF': ('Lockbox batch reference', X, 'IND', ''),
})
t('BIL_DISTRIBUTION', 'BC', 'DirectBillPayment → PaymentItem (BaseDistItem)', 'Allocation of money to invoice items.', {
  'DIST_AMOUNT': ('BaseDistItem.GrossAmountToApply', D, 'DM', ''), 'DIST_DATE': ('BaseDistItem.ExecutedDate', D, 'IND', ''),
  'REVERSED_FLAG': ('BaseDistItem.ReversedDate (derived)', V, 'IND', ''), 'REVERSED_DIST_ID': ('reversal link', V, 'IND', ''),
})
t('BIL_SUSPENSE', 'BC', 'SuspensePayment', 'Unidentified money.', {
  'SUSP_AMOUNT': ('SuspensePayment.Amount', D, 'DM', ''), 'REF_TEXT': ('SuspensePayment.Description / RefNumber', X, 'IND', ''),
  'SUSP_REASON': ('SuspensePayment reason', X, 'IND', ''), 'SUSP_STATUS': ('SuspensePayment.Status', X, 'DM', 'OPEN→open, MTCH→applied, RVSD→reversed'),
  'MATCHED_ACCOUNT_ID': ('SuspensePayment.AccountNumber (on apply)', X, 'IND', ''),
  'CREATE_DATE': ('SuspensePayment.PaymentDate', D, 'IND', ''), 'RESOLVED_DATE': ('apply/reverse date', D, 'IND', ''), 'RESOLVED_USER': ('apply user', D, 'IND', ''),
})
t('BIL_PAYMENT_REQUEST', 'BC', 'PaymentRequest', 'Direct-debit drafts.', {
  'REQUEST_AMOUNT': ('PaymentRequest.Amount', D, 'DM', ''), 'DRAFT_DATE': ('PaymentRequest.DraftDate', D, 'DM', ''),
  'REQUEST_STATUS': ('PaymentRequest.Status', X, 'DM', 'CRTD→created, SENT→requested, SETL→closed, RTND→canceled'),
  'RETURN_CODE': ('Bank return reason', X, 'IND', ''), 'FILE_REF': ('outbound file reference', X, 'IND', ''),
})
t('BIL_DELINQ_PROCESS', 'BC', 'DelinquencyProcess (PolicyDlnqProcess)', 'Open delinquency on a policy period.', {
  'PLAN_CODE': ('DelinquencyProcess.DelinquencyPlan', X, 'DM', ''), 'START_DATE': ('DelinquencyProcess.StartDate', D, 'DM', ''),
  'PASTDUE_AMOUNT': ('DelinquencyProcess past-due amount (derived)', V, 'IND', ''),
  'CURRENT_STEP': ('DelinquencyProcess workflow step', V, 'DM', ''),
  'DP_STATUS': ('DelinquencyProcess.Status', X, 'DM', 'OPEN→open, CLSD→closed'),
  'EXIT_REASON': ('DelinquencyProcess.ExitReason', X, 'IND', 'PAID→paid'), 'EXIT_DATE': ('DelinquencyProcess.ExitDate', D, 'IND', ''),
})
t('BIL_DELINQ_EVENT', 'BC', 'DelinquencyProcessEvent', 'Executed / scheduled steps.', {
  'EVENT_CODE': ('DelinquencyProcessEvent.EventName', X, 'DM', ''), 'TARGET_DATE': ('DelinquencyProcessEvent.TargetDate', D, 'DM', ''),
  'COMPLETED_DATE': ('DelinquencyProcessEvent.CompletionTime', D, 'DM', ''), 'EVENT_STATUS': ('DelinquencyProcessEvent.Completed', X, 'IND', ''),
  'RESULT_REF': ('outcome reference', X, 'IND', ''),
})
t('BIL_WRITEOFF', 'BC', 'Writeoff (ChargeGrossWriteoff)', 'Write-offs.', {
  'WO_AMOUNT': ('Writeoff.Amount', D, 'DM', ''), 'WO_REASON': ('Writeoff.Reason', X, 'DM', 'SMALLBAL→minorAdjustment, DELINQ→uncollectable'),
  'WO_STATUS': ('Writeoff approval status', X, 'IND', 'PEND → approval activity'), 'REQUEST_USER': ('CreateUser', V, 'DM', ''),
  'APPROVER_USER': ('Approval activity approver', X, 'IND', ''), 'WO_DATE': ('Writeoff.ExecutionDate', D, 'IND', ''),
  'REVERSED_FLAG': ('WriteoffReversal (derived)', V, 'IND', ''),
})
t('BIL_DISBURSEMENT', 'BC', 'Disbursement (AccountDisbursement)', 'Outgoing payments.', {
  'DISB_AMOUNT': ('Disbursement.Amount', D, 'DM', ''), 'DISB_REASON': ('Disbursement.Reason', X, 'DM', 'OVERPAY→overpayment, REFUND→refund; CLMPAY is a gap (see note)'),
  'PAYEE_NAME': ('Disbursement.PayTo', D, 'DM', ''), 'PAYEE_ADDR1': ('Disbursement.MailTo / address', X, 'IND', ''),
  'PAYEE_POSTCODE': ('Disbursement address postal code', X, 'IND', ''), 'PAY_METHOD': ('Disbursement payment method', X, 'IND', ''),
  'DUE_DATE': ('Disbursement.DueDate', D, 'DM', ''), 'DISB_STATUS': ('Disbursement.Status', X, 'DM', 'PEND→awaitingapproval, APPR→approved, SENT→sent, VOID→voided'),
  'SOURCE_SYSTEM': ('SourceSystem_Ext', E, 'EXT', 'In Guidewire, claim payments are issued by ClaimCenter, not BillingCenter: CLMS rows migrate to CC checks instead'),
  'SOURCE_REF': ('SourceRef_Ext', E, 'EXT', ''), 'APPROVER_USER': ('approval activity', X, 'IND', ''),
  'SENT_DATE': ('Disbursement.CloseDate (sent)', D, 'IND', ''), 'VOID_REASON': ('Disbursement.VoidReason', X, 'IND', ''),
})
t('BIL_PRODUCER', 'BC', 'Producer + ProducerCode', 'Brokers.', {
  'PRODUCER_CODE': ('ProducerCode.Code', X, 'DM', 'Same value as GenApp BROKERID and PolicyCenter producer code'),
  'PRODUCER_NAME': ('Producer.Name', D, 'DM', ''), 'PRODUCER_TIER': ('Producer.Tier', X, 'DM', 'STD/GOLD/PLAT → bronze/silver/gold'),
  'COMM_PLAN_CODE': ('ProducerCode.CommissionPlan', X, 'DM', ''), 'PAY_CYCLE': ('Producer.PaymentCycle (recurring periodicity)', X, 'IND', ''),
  'AGENCY_BILL_FLAG': ('Producer.AgencyBillPlan', X, 'DM', 'Y → agency bill plan assigned'),
  'PAYABLE_BALANCE': ('Producer commission payable (derived)', V, 'DM', ''), 'PRODUCER_STATUS': ('ProducerCode.Active', X, 'IND', ''),
})
t('BIL_COMMISSION_ITEM', 'BC', 'ItemCommission', 'Commission per invoice item.', {
  'BASIS_AMOUNT': ('ItemCommission basis (item amount)', V, 'IND', ''), 'RATE_BPS': ('ItemCommission.CommissionRate', X, 'DM', 'bps → percent'),
  'COMM_AMOUNT': ('ItemCommission.CommissionAmount', D, 'DM', ''), 'EARNED_AMOUNT': ('ItemCommission earned (derived)', V, 'DM', ''),
  'CI_STATE': ('commission state (reserve/earned/payable/paid)', X, 'IND', 'T-account driven in BC'),
})
t('BIL_PRODUCER_PAYMENT', 'BC', 'OutgoingProducerPayment', 'Producer payments.', {
  'PERIOD_END': ('ProducerPayment.PaymentDate', D, 'IND', ''), 'PAY_AMOUNT': ('OutgoingProducerPayment.Amount', D, 'DM', ''),
  'ITEM_COUNT': ('derived', V, '--', ''), 'PAY_STATUS': ('OutgoingProducerPayment.Status', X, 'IND', ''), 'DISB_ID': ('payment link', V, 'IND', ''),
})
t('BIL_AGENCY_STATEMENT', 'BC', 'StatementInvoice (agency bill)', 'Agency bill statements.', {
  'STMT_DATE': ('StatementInvoice.EventDate', D, 'IND', ''), 'DUE_DATE': ('StatementInvoice.PaymentDueDate', D, 'IND', ''),
  'GROSS_AMOUNT': ('StatementInvoice gross (derived)', V, 'IND', ''), 'COMMISSION_AMOUNT': ('StatementInvoice commission (derived)', V, 'IND', ''),
  'NET_DUE': ('StatementInvoice.AmountDue', V, 'IND', ''), 'PAID_AMOUNT': ('agency payment applied', V, 'IND', ''),
  'STMT_STATUS': ('StatementInvoice.Status', X, 'IND', ''), 'ITEM_COUNT': ('derived', V, '--', ''),
})
t('BIL_TRANSACTION', 'BC', 'Transaction (BillingCenter financial transaction)', 'Ledger transactions are generated by BillingCenter itself.', {
  'TXN_TYPE': ('Transaction subtype', X, 'IND', 'Rebuilt by BC; legacy ledger loaded only as opening balances'),
  'TXN_AMOUNT': ('Transaction.Amount', V, 'DM', ''), 'BUSINESS_DATE': ('Transaction.TransactionDate', V, 'DM', ''),
  'REF_TYPE': ('owner link', V, '--', ''), 'REF_ID': ('owner link', V, '--', ''), 'REVERSAL_OF_TXN': ('Transaction reversal link', V, 'IND', ''),
})
t('BIL_LEDGER_LINE', 'BC', 'LineItem / TAccount', 'Double-entry lines.', {
  'T_ACCOUNT': ('TAccount (pattern)', X, 'DM', 'UNBILLED/BILLED/DUE/UNAPPLD/CASH… map to BC T-account patterns'),
  'DR_CR': ('LineItem.Type (debit/credit)', D, 'DM', ''), 'LINE_AMOUNT': ('LineItem.Amount', D, 'DM', ''),
  'BUSINESS_DATE': ('Transaction date', V, 'DM', ''), 'LINE_SEQ': ('order', V, '--', ''),
})
t('BIL_AUDIT', 'BC', 'History (AccountHistory / PolicyPeriodHistory)', 'Field-level audit.', {
  'ENTITY_NAME': ('History subtype', X, 'IND', ''), 'ENTITY_KEY': ('History owner', X, 'IND', ''),
  'ACTION_CODE': ('History.EventType', X, 'IND', ''), 'FIELD_NAME': ('History description', X, 'IND', ''),
  'BEFORE_VALUE': ('History description (before)', X, 'IND', ''), 'AFTER_VALUE': ('History description (after)', X, 'IND', ''),
  'USER_ID': ('History.User', D, 'DM', ''), 'PROGRAM_ID': ('(not migrated)', R, '--', ''), 'AUDIT_TS': ('History.EventDate', D, 'DM', ''),
})
t('BIL_EVENT_QUEUE', 'BC', 'Messaging (MessageDestination / Message)', 'Integration events become GW messaging or App Events.', {
  'EVENT_TYPE': ('Message event name', X, 'IND', 'POLISSUE etc. replaced by PolicyCenter→BillingCenter billing-instruction API'),
  'PAYLOAD': ('Message.Payload', X, 'IND', 'Fixed-position text → JSON / Cloud API'),
  'EVENT_STATUS': ('Message.Status', X, 'DM', ''), 'RETRY_COUNT': ('Message.RetryCount', D, 'IND', ''),
  'ERROR_TEXT': ('Message.ErrorDescription', D, 'IND', ''),
})
t('BIL_TROUBLE_TICKET', 'BC', 'TroubleTicket', 'Customer-service trouble tickets.', {
  'TICKET_TYPE': ('TroubleTicket.TicketType', X, 'DM', ''), 'PRIORITY': ('TroubleTicket.Priority', X, 'DM', ''),
  'SUBJECT': ('TroubleTicket.Title', D, 'DM', ''), 'TICKET_STATUS': ('TroubleTicket closed (derived)', X, 'IND', ''),
  'ASSIGNED_USER': ('TroubleTicket.AssignedUser', D, 'DM', ''), 'HOLD_ID': ('TroubleTicket.Hold', X, 'DM', ''),
  'DUE_DATE': ('TroubleTicket.TargetDate', D, 'DM', ''), 'CLOSE_TS': ('TroubleTicket.CloseDate', D, 'DM', ''),
})
t('BIL_ERROR_LOG', 'BC', 'Platform logging', 'Technical error log.', {})

# ======================================================= ClaimCenter
t('CLM_CONTROL', 'CC', 'Platform (batch processing / system clock)', 'Business date and batch state are server-managed.', {
  'BUSINESS_DATE': ('System clock', R, '--', ''), 'CURRENCY_CODE': ('default currency config', X, 'DM', ''),
})
t('CLM_SEQUENCE', 'CC', 'Platform (numbering plugins)', 'Claim and check numbering via ClaimCenter plugins.', {
  'SEQ_NAME': ('IClaimNumGenAdapter / check numbering', X, 'IND', 'CLMNO → claim number generator; CHKNO → check numbering'),
})
t('CLM_CODE_TABLE', 'CC', 'Typelists', 'Codes → typecodes; PARENT_CODE → typelist category filter.', {
  'TABLE_NAME': ('Typelist name', X, 'DM', ''), 'CODE_VALUE': ('Typecode code', X, 'DM', ''),
  'DESCRIPTION': ('Typecode name', D, 'DM', ''), 'PARENT_CODE': ('Typecode category (filter by parent typelist)', X, 'DM', 'LOSSCAUS filtered by LossType category'),
  'SORT_ORDER': ('Typecode priority', D, 'DM', ''), 'ACTIVE_FLAG': ('Typecode retired', X, 'DM', ''),
})
t('CLM_GROUP', 'CC', 'Group', 'Claims organisation.', {
  'GROUP_NAME': ('Group.Name', D, 'DM', ''), 'PARENT_GROUP': ('Group.Parent', X, 'DM', ''), 'GROUP_TYPE': ('Group.GroupType', X, 'DM', ''),
  'SUPERVISOR_ID': ('Group.Supervisor', X, 'DM', ''), 'REGION_CODE': ('GroupRegion → Region', X, 'DM', ''),
})
t('CLM_USER', 'CC', 'User', 'Adjusters and supervisors.', {
  'USER_ID': ('Credential.UserName', D, 'DM', ''), 'USER_NAME': ('User contact name', X, 'DM', ''),
  'GROUP_CODE': ('GroupUser', X, 'DM', ''), 'AUTH_PROFILE': ('User.AuthorityProfile', X, 'DM', ''),
  'SUPERVISOR_ID': ('Group.Supervisor', X, 'DM', ''), 'LOB_SKILLS': ('UserAttribute (LOB)', X, 'IND', ''),
  'OPEN_CLAIMS': ('workload (derived)', V, 'DM', 'ClaimCenter load balancing computes this'),
  'MAX_CLAIMS': ('Group load factor', X, 'IND', ''), 'LAST_ASSIGN_TS': ('round-robin state (platform)', R, '--', ''),
})
t('CLM_AUTHORITY_LIMIT', 'CC', 'AuthorityLimitProfile → AuthorityLimit', 'Financial authority.', {
  'AUTH_PROFILE': ('AuthorityLimitProfile', X, 'DM', ''),
  'LIMIT_TYPE': ('AuthorityLimit.LimitType', X, 'DM', 'RSVEXP→exposurereserve, PAYEXP→exposurepayment, PAYCLM→claimpayment, INCCLM→claimtotal'),
  'COVERAGE_TYPE': ('AuthorityLimit.CoverageType', X, 'DM', "'*' → limit without coverage type"), 'LIMIT_AMOUNT': ('AuthorityLimit.LimitAmount', D, 'DM', ''),
})
t('CLM_ASSIGN_RULE', 'CC', 'Assignment rules (Gosu rule set)', 'Table-driven rules become ClaimCenter assignment rules / AssignmentStrategy.', {
  'LOSS_TYPE': ('rule condition', X, 'IND', ''), 'SEGMENT_CODE': ('rule condition', X, 'IND', ''), 'MIN_SEVERITY': ('rule condition', X, 'IND', ''),
  'REGION_CODE': ('rule condition', X, 'IND', ''), 'GROUP_CODE': ('assignment target group', X, 'IND', ''),
  'STRATEGY': ('assignByRoundRobin / load balancing', X, 'IND', 'RRBN→round robin, LOAD→load balanced'),
})
t('CLM_ACTIVITY_PATTERN', 'CC', 'ActivityPattern', 'Activity templates.', {
  'PATTERN_CODE': ('ActivityPattern.Code', D, 'DM', ''), 'SUBJECT': ('ActivityPattern.Subject', D, 'DM', ''),
  'CATEGORY': ('ActivityPattern.Category', X, 'DM', ''), 'TRIGGER_EVENT': ('Rule that generates the activity', X, 'IND', 'Becomes event rules / workplan'),
  'TARGET_DAYS': ('ActivityPattern.TargetDays', D, 'DM', ''), 'ESCALATION_DAYS': ('ActivityPattern.EscalationDays', D, 'DM', ''),
  'PRIORITY': ('ActivityPattern.Priority', X, 'DM', ''), 'MANDATORY_FLAG': ('ActivityPattern.Mandatory', X, 'DM', ''),
})
t('CLM_SIU_RULE', 'CC', 'SIU rules (Gosu rule set)', 'Fraud scoring rules.', {
  'RULE_CODE': ('SIU rule name', X, 'IND', ''), 'POINTS': ('SIU score contribution', X, 'IND', ''),
})
t('CLM_CATASTROPHE', 'CC', 'Catastrophe', 'Catastrophe events.', {
  'CAT_CODE': ('Catastrophe.CatastropheNumber', D, 'DM', ''), 'CAT_NAME': ('Catastrophe.Name', D, 'DM', ''),
  'CAT_TYPE': ('Catastrophe.Type', X, 'DM', ''), 'START_DATE': ('Catastrophe.CatastropheValidFrom', D, 'DM', ''),
  'END_DATE': ('Catastrophe.CatastropheValidTo', D, 'DM', ''), 'POSTCODE_AREA': ('CatastropheZone (postal code)', X, 'DM', ''),
})
t('CLM_RI_TREATY', 'CC', 'Reinsurance agreement (RI integration)', 'ClaimCenter base has no RI treaty administration; usually integrated with an RI system.', {
  'TREATY_CODE': ('RIAgreement_Ext / external RI system', G, '--', 'Keep in reinsurance system; CC holds ceded amounts only if integrated'),
})
t('CLM_CLAIM', 'CC', 'Claim', 'The claim.', {
  'CLAIM_NUMBER': ('Claim.ClaimNumber', D, 'DM', 'Legacy number kept via claim number generator'),
  'GENAPP_POL_NUM': ('Claim.Policy.PolicyNumber', X, 'DM', 'Verified policy retrieved from PolicyCenter'),
  'GENAPP_CUST_NUM': ('Claim.Insured (contact)', X, 'DM', ''),
  'POLICY_TYPE': ('Claim.Policy.PolicyType', X, 'DM', 'M→PersonalAuto, H→Homeowners, C→CommercialProperty'),
  'LOSS_TYPE': ('Claim.LossType', X, 'DM', 'AUTO→AUTO, PROP→PR, LIAB→GL'),
  'LOSS_DATE': ('Claim.LossDate', X, 'DM', 'Date + LOSS_TIME → datetime'), 'LOSS_TIME': ('Claim.LossDate (time part)', X, 'DM', ''),
  'REPORTED_DATE': ('Claim.ReportedDate', D, 'DM', ''), 'HOW_REPORTED': ('Claim.HowReported', X, 'DM', ''),
  'LOSS_CAUSE': ('Claim.LossCause', X, 'DM', 'Lower-case typecodes; category = loss type'),
  'LOSS_DESC': ('Claim.Description', D, 'DM', ''), 'LOSS_ADDR_LINE1': ('Claim.LossLocation.AddressLine1', D, 'DM', ''),
  'LOSS_POSTCODE': ('Claim.LossLocation.PostalCode', D, 'DM', ''),
  'CLAIM_STATE': ('Claim.State', X, 'DM', 'DRFT→draft, OPEN→open, CLSD→closed'),
  'CLOSE_REASON': ('Claim.ClosedOutcome', X, 'DM', 'COMPLETE→completed, DUPLICAT→duplicate, DENIED→denial, WITHDRAW→withdrawn, NOPAY→nopayment'),
  'SEGMENT_CODE': ('Claim.Segment', X, 'DM', ''), 'SEVERITY': ('Claim severity (Incident.Severity / Claim.Flagged)', X, 'IND', 'CC holds severity on incidents'),
  'FAULT_RATING': ('Claim.FaultRating', X, 'DM', 'INSD→1 (insured at fault), THRD→2, SHRD→3, NOFT→4, UNKN→5'),
  'COVERAGE_IN_QSTN': ('Claim.CoverageInQuestion', X, 'DM', 'Y/N → boolean'),
  'SIU_SCORE': ('Claim.SIScore', D, 'IND', ''), 'SIU_STATUS': ('Claim.SIUStatus', X, 'DM', 'REFR→referred'),
  'LITIGATION_FLAG': ('Claim.LitigationStatus', X, 'DM', 'Derived from open Matters'),
  'CAT_CODE': ('Claim.Catastrophe', X, 'DM', ''), 'TOTAL_LOSS_FLAG': ('VehicleIncident.TotalLoss (claim-level flag derived)', X, 'DM', ''),
  'DUP_SUSPECT_FLAG': ('Claim duplicate-claim check (rules)', X, 'IND', 'Duplicate check runs in CC rules / ISO'),
  'ASSIGNED_USER': ('Claim.AssignedUser', D, 'DM', ''), 'ASSIGNED_GROUP': ('Claim.AssignedGroup', D, 'DM', ''),
  'TOTAL_RESERVES': ('Claim financials (derived)', V, 'DM', 'Computed by CC financials calculations'),
  'TOTAL_PAID': ('Claim financials (derived)', V, 'DM', ''), 'TOTAL_RECOVERED': ('Claim financials (derived)', V, 'DM', ''),
  'TOTAL_INCURRED': ('Claim financials (derived)', V, 'DM', ''),
  'OPEN_DATE': ('Claim.CreateTime / open date', D, 'IND', ''), 'CLOSE_DATE': ('Claim.CloseDate', D, 'DM', ''),
  'REOPEN_COUNT': ('derived from history', V, 'IND', ''),
})
t('CLM_POLICY_SNAPSHOT', 'CC', 'Policy (verified claim policy)', 'Policy as retrieved for the loss date.', {
  'POLICY_NUMBER': ('Policy.PolicyNumber', D, 'DM', ''), 'POLICY_TYPE': ('Policy.PolicyType', X, 'DM', ''),
  'EFF_DATE': ('Policy.EffectiveDate', D, 'DM', ''), 'EXP_DATE': ('Policy.ExpirationDate', D, 'DM', ''),
  'VERIFIED_FLAG': ('Policy.Verified', X, 'DM', ''), 'INSURED_NAME': ('Policy insured (ClaimContact role)', X, 'DM', ''),
  'PRODUCER_CODE': ('Policy.ProducerCode', D, 'DM', ''), 'STATUS_AT_LOSS': ('Policy.Status', X, 'DM', ''),
  'BILLING_STATUS': ('BillingStatus_Ext (via BC summary integration)', E, 'EXT', 'CC can show billing summary from BillingCenter'),
  'SNAPSHOT_TS': ('Policy retrieval time', V, 'IND', ''),
})
t('CLM_POLICY_COVERAGE', 'CC', 'PolicyCoverage / RUCoverage', 'Coverages on the claim policy.', {
  'COVERAGE_TYPE': ('Coverage.Type', X, 'DM', 'COLL/COMP/LIAB_BI/LIAB_PD/DWELLING/CONTENTS/BLDG/BUSINT/MEDPAY → CC coverage typecodes'),
  'LIMIT_AMOUNT': ('Coverage.ExposureLimit / IncidentLimit', X, 'DM', ''), 'DEDUCTIBLE_AMOUNT': ('Coverage.Deductible', D, 'DM', ''),
  'RISK_UNIT_SEQ': ('RUCoverage.RiskUnit', V, 'DM', ''),
})
t('CLM_RISK_UNIT', 'CC', 'RiskUnit (VehicleRU / PropertyRU)', 'Insured objects on the policy.', {
  'RISK_UNIT_TYPE': ('RiskUnit subtype', X, 'DM', 'VEH→VehicleRU, DWEL/BLDG→PropertyRU'),
  'DESCRIPTION': ('RiskUnit.Description', D, 'DM', ''), 'VEHICLE_REG': ('Vehicle.LicensePlate', D, 'DM', ''),
  'VEHICLE_MAKE': ('Vehicle.Make', D, 'DM', ''), 'VEHICLE_MODEL': ('Vehicle.Model', D, 'DM', ''),
  'PROPERTY_POSTCODE': ('PolicyLocation.Address.PostalCode', D, 'DM', ''), 'INSURED_VALUE': ('Vehicle.Value / property value', X, 'IND', ''),
})
t('CLM_CONTACT', 'CC', 'Contact (Person / Company / vendor subtypes) — synced with ContactManager', 'Claim contacts.', {
  'CONTACT_TYPE': ('Contact subtype', X, 'DM', 'PERS→Person, COMP→Company, vendors→AutoRepairShop/Attorney/… by VENDOR_TYPE'),
  'FIRST_NAME': ('Person.FirstName', D, 'DM', ''), 'LAST_NAME': ('Person.LastName', D, 'DM', ''), 'COMPANY_NAME': ('Company.Name', D, 'DM', ''),
  'DATE_OF_BIRTH': ('Person.DateOfBirth', D, 'DM', ''), 'ADDRESS_LINE1': ('Address.AddressLine1', D, 'DM', ''),
  'CITY': ('Address.City', D, 'DM', ''), 'POSTCODE': ('Address.PostalCode', D, 'DM', ''),
  'PHONE_NUMBER': ('Contact.PrimaryPhone', X, 'DM', ''), 'EMAIL_ADDRESS': ('Contact.EmailAddress1', D, 'DM', ''),
  'TAX_ID_TOKEN': ('Contact.TaxID (tokenised)', X, 'DM', ''), 'GENAPP_CUST_NUM': ('Contact link to PolicyCenter account holder', X, 'IND', ''),
  'VENDOR_TYPE': ('Contact subtype (vendor)', X, 'DM', ''), 'PREFERRED_FLAG': ('Contact.Preferred', X, 'DM', ''),
  'VENDOR_RATING': ('Contact.Score / review', X, 'IND', ''), 'SANCTION_STATUS': ('SanctionScreen_Ext (OFAC/HMT integration)', E, 'EXT', ''),
})
t('CLM_CLAIM_CONTACT', 'CC', 'ClaimContact + ClaimContactRole', 'Roles of contacts on the claim.', {
  'CONTACT_ROLE': ('ClaimContactRole.Role', X, 'DM', 'INSURED→insured, CLAIMANT→claimant, REPORTER→reporter, WITNESS→witness, ATTORNEY→attorney, ADVERSE→other party'),
  'EXPOSURE_ID': ('ClaimContactRole.Exposure', X, 'DM', ''), 'ADDED_TS': ('CreateTime', V, '--', ''),
})
t('CLM_INCIDENT', 'CC', 'Incident (VehicleIncident / InjuryIncident / PropertyIncident / FixedPropertyIncident)', 'Incidents.', {
  'INCIDENT_TYPE': ('Incident subtype', X, 'DM', 'VEH→VehicleIncident, INJ→InjuryIncident, PROP→PropertyIncident, FIXP→FixedPropertyIncident'),
  'DESCRIPTION': ('Incident.Description', D, 'DM', ''), 'SEVERITY': ('Incident.Severity', X, 'DM', ''),
  'VEHICLE_REG': ('VehicleIncident.Vehicle.LicensePlate', D, 'DM', ''), 'DRIVEABLE_FLAG': ('VehicleIncident.Driveable', X, 'DM', ''),
  'ESTIMATED_DAMAGE': ('Incident.EstRepairCost', D, 'DM', ''), 'TOTAL_LOSS_FLAG': ('VehicleIncident.TotalLoss', X, 'DM', ''),
  'INJURY_TYPE': ('InjuryIncident.GeneralInjuryType', X, 'DM', ''), 'INJURED_CONTACT_ID': ('InjuryIncident.InjuredPerson', X, 'DM', ''),
  'RISK_UNIT_SEQ': ('VehicleIncident.Vehicle (policy vehicle)', X, 'IND', ''),
})
t('CLM_EXPOSURE', 'CC', 'Exposure', 'Exposures.', {
  'EXPOSURE_TYPE': ('Exposure.ExposureType', X, 'DM', 'VEHDAMAG→VehicleDamage, PROPDAMA→PropertyDamage, BODINJUR→BodilyInjuryDamage, MEDPAY→MedPay, CONTENTS→Content, LOSSOFUS→LossOfUseDamage, GENDAMAG→GeneralDamage'),
  'COVERAGE_TYPE': ('Exposure.PrimaryCoverage', X, 'DM', ''), 'COV_SEQ': ('Exposure.Coverage', X, 'DM', ''),
  'CLAIMANT_ID': ('Exposure.Claimant', X, 'DM', ''), 'INCIDENT_ID': ('Exposure.Incident', X, 'DM', ''),
  'EXPOSURE_STATE': ('Exposure.State', X, 'DM', ''), 'VALIDATION_LEVEL': ('Exposure.ValidationLevel', X, 'DM', 'NEWL→newloss, ABLE→ability to pay'),
  'SEGMENT_CODE': ('Exposure.Segment', X, 'DM', ''), 'ASSIGNED_USER': ('Exposure.AssignedUser', D, 'DM', ''),
  'OPEN_RESERVE': ('Exposure financials (derived)', V, 'DM', ''), 'TOTAL_PAID': ('Exposure financials (derived)', V, 'DM', ''),
  'TOTAL_RECOVERED': ('Exposure financials (derived)', V, 'DM', ''), 'DEDUCTIBLE_APPLIED': ('Deductible.Paid / Waived', X, 'DM', 'CC Deductible entity tracks applied deductible'),
  'CREATE_DATE': ('Exposure.CreateTime', V, 'DM', ''), 'CLOSE_DATE': ('Exposure.CloseDate', D, 'DM', ''), 'CLOSE_REASON': ('Exposure.ClosedOutcome', X, 'DM', ''),
  'EXPOSURE_SEQ': ('Exposure.ClaimOrder', D, 'DM', ''),
})
t('CLM_RESERVE_LINE', 'CC', 'ReserveLine', 'Reserve lines (exposure × cost type × category).', {
  'COST_TYPE': ('ReserveLine.CostType', X, 'DM', 'CLMC→claimcost, EXAO→aoexpense, EXDC→dccexpense'),
  'COST_CATEGORY': ('ReserveLine.CostCategory', X, 'DM', ''),
  'OPEN_RESERVE': ('open reserves (derived)', V, 'DM', 'Computed from transactions'), 'PENDING_APPROVAL': ('pending approval (derived)', V, 'DM', ''),
  'TOTAL_PAID': ('derived', V, 'DM', ''), 'RECOVERY_RESERVE': ('derived', V, 'DM', ''), 'TOTAL_RECOVERED': ('derived', V, 'DM', ''),
})
t('CLM_TRANSACTION', 'CC', 'Transaction (Reserve / Payment / RecoveryReserve / Recovery) + TransactionLineItem', 'Financial transactions.', {
  'TXN_TYPE': ('Transaction subtype', X, 'DM', 'RSV→Reserve, PAY→Payment, RRSV→RecoveryReserve, RCV→Recovery, DED→Payment deduction (Deductible)'),
  'TXN_STATUS': ('Transaction.Status', X, 'DM', 'SUBM→submitted, PAPR→pendingapproval, REJT→rejected, VOID→voided'),
  'TXN_AMOUNT': ('TransactionLineItem.TransactionAmount', D, 'DM', ''), 'ERODES_RESERVE': ('Payment.ErodesReserves', X, 'DM', ''),
  'PAYMENT_TYPE': ('Payment.PaymentType', X, 'DM', 'PART→partial, FINL→final, SUPP→supplement'),
  'RECOVERY_CATEGORY': ('Recovery.RecoveryCategory', X, 'DM', 'SUBRO→subro, SALVAGE→salvage, DEDUCT→deductible, OVERPAY→overpayment'),
  'CHECK_ID': ('Payment.Check', X, 'DM', ''), 'CREATE_USER': ('Transaction.CreateUser', V, 'DM', ''),
  'APPROVER_USER': ('Transaction approval (Activity approval)', X, 'DM', ''), 'REJECT_REASON': ('approval rejection reason', X, 'IND', ''),
  'RELATED_TXN_ID': ('offset / onset link', X, 'IND', ''), 'BUSINESS_DATE': ('Transaction.CreateTime (accounting date)', V, 'IND', ''),
  'APPROVED_TS': ('Transaction.ApprovalDate', D, 'IND', ''),
})
t('CLM_CHECK', 'CC', 'Check', 'Checks and EFTs.', {
  'CHECK_NUMBER': ('Check.CheckNumber', D, 'DM', ''), 'PAYEE_CONTACT_ID': ('Check.Payees (CheckPayee)', X, 'DM', ''),
  'PAYEE_NAME': ('Check.PayTo', D, 'DM', ''), 'PAYEE_ADDR1': ('Check.MailToAddress', X, 'DM', ''), 'PAYEE_POSTCODE': ('Check.MailToAddress.PostalCode', X, 'DM', ''),
  'CHECK_AMOUNT': ('Check.GrossAmount', D, 'DM', ''), 'PAY_METHOD': ('Check.PaymentMethod', X, 'DM', 'CHEQ→check, BACS/FAST→eft'),
  'SCHEDULED_DATE': ('Check.ScheduledSendDate', D, 'DM', ''), 'ISSUE_DATE': ('Check.IssueDate', D, 'DM', ''),
  'CHECK_STATUS': ('Check.Status', X, 'DM', 'HOLD→pendingapproval, PEND→awaitingsubmission, ISSU→issued, CLRD→cleared, VOID→voided'),
  'BILLING_DISB_REF': ('(replaced by CC check issuance to payments system)', R, '--', 'In Guidewire, CC issues checks itself; BillingCenter is not in the claim-payment path'),
  'VOID_REASON': ('Check void reason', X, 'IND', ''),
})
t('CLM_SUBROGATION', 'CC', 'SubrogationSummary', 'Subrogation.', {
  'SUBRO_STATUS': ('SubrogationSummary.Status', X, 'DM', ''), 'ADVERSE_PARTY_ID': ('SubrogationSummary responsible party', X, 'IND', ''),
  'ADVERSE_CARRIER': ('responsible party carrier', X, 'IND', ''), 'LIABILITY_PCT': ('SubrogationSummary liability %', X, 'IND', ''),
  'EST_RECOVERY': ('SubrogationSummary.EstimatedRecovery', X, 'IND', ''), 'OPENED_DATE': ('subrogation open date', D, 'IND', ''),
  'CLOSED_DATE': ('subrogation close date', D, 'IND', ''), 'CLOSE_OUTCOME': ('SubrogationSummary outcome', X, 'IND', ''),
})
t('CLM_SALVAGE', 'CC', 'VehicleIncident salvage fields', 'Salvage of total-loss vehicles.', {
  'SALVAGE_STATUS': ('VehicleIncident.SalvageStatus', X, 'IND', ''), 'VENDOR_CONTACT_ID': ('salvage company (ServiceRequest specialist)', X, 'IND', ''),
  'EST_VALUE': ('VehicleIncident.SalvageNet (estimate)', X, 'IND', ''), 'SALE_AMOUNT': ('Salvage recovery (Recovery)', X, 'DM', ''),
  'ASSIGNED_DATE': ('salvage assignment date', D, 'IND', ''), 'SALE_DATE': ('salvage sale date', D, 'IND', ''),
})
t('CLM_LITIGATION', 'CC', 'Matter', 'Litigation matters.', {
  'MATTER_TYPE': ('Matter.MatterType', X, 'DM', 'SUIT→lawsuit, ARBITRAT→arbitration, MEDIATN→mediation'),
  'COURT_NAME': ('Matter.CourtType / venue', X, 'IND', ''), 'CASE_NUMBER': ('Matter.CaseNumber', D, 'DM', ''),
  'PLAINTIFF_ATTY_ID': ('Matter plaintiff attorney (ClaimContactRole)', X, 'DM', ''), 'DEFENSE_ATTY_ID': ('Matter defense attorney', X, 'DM', ''),
  'FILE_DATE': ('Matter.FileDate', D, 'DM', ''), 'TRIAL_DATE': ('Matter.TrialDate', D, 'DM', ''),
  'MATTER_STATUS': ('Matter.CloseDate (derived)', X, 'IND', ''), 'RESOLUTION': ('Matter.Resolution', X, 'DM', 'SETTLED→settled, JUDGMNT→judgment, DISMISSD→dismissed'),
  'SETTLEMENT_AMOUNT': ('Matter.FinalSettleAmount', D, 'DM', ''), 'LEGAL_SPEND': ('derived from DCC expense payments', V, 'IND', ''),
  'CLOSED_DATE': ('Matter.CloseDate', D, 'DM', ''),
})
t('CLM_SIU_SCORE', 'CC', 'SIU indicators (rule results)', 'Per-rule fraud score hits.', {
  'RULE_CODE': ('SIUIndicator_Ext / rule hit', E, 'EXT', ''), 'POINTS': ('score contribution', E, 'EXT', ''),
  'EVIDENCE_TEXT': ('note on claim', X, 'IND', ''), 'EVAL_TS': ('evaluation time', V, '--', ''),
})
t('CLM_RI_CESSION', 'CC', 'Reinsurance cession (RI integration)', 'Ceded amounts per treaty.', {
  'TREATY_CODE': ('RI system', G, '--', 'Base CC has no RI cession entities'), 'GROSS_INCURRED': ('RI system', G, '--', ''),
  'RETAINED_AMOUNT': ('RI system', G, '--', ''), 'CEDED_AMOUNT': ('RI system', G, '--', ''),
  'NOTIFIED_FLAG': ('RI system', G, '--', ''), 'CALC_DATE': ('RI system', G, '--', ''),
})
t('CLM_ACTIVITY', 'CC', 'Activity', 'Diary / tasks.', {
  'PATTERN_CODE': ('Activity.ActivityPattern', X, 'DM', ''), 'SUBJECT': ('Activity.Subject', D, 'DM', ''),
  'PRIORITY': ('Activity.Priority', X, 'DM', ''), 'ACTIVITY_STATUS': ('Activity.Status', X, 'DM', 'OPEN→open, CMPL→complete, SKIP→skipped'),
  'ASSIGNED_USER': ('Activity.AssignedUser', D, 'DM', ''), 'DUE_DATE': ('Activity.TargetDate', D, 'DM', ''),
  'ESCALATION_DATE': ('Activity.EscalationDate', D, 'DM', ''), 'ESCALATED_FLAG': ('Activity.Escalated', X, 'DM', ''),
  'APPROVAL_TXN_ID': ('Activity.TransactionSet (approval)', X, 'DM', ''), 'COMPLETE_TS': ('Activity.CloseDate', D, 'DM', ''),
  'COMPLETE_USER': ('Activity.CloseUser', D, 'IND', ''), 'EXPOSURE_ID': ('Activity.Exposure', X, 'DM', ''),
})
t('CLM_NOTE', 'CC', 'Note', 'Claim notes.', {
  'NOTE_TOPIC': ('Note.Topic', X, 'DM', ''), 'SUBJECT': ('Note.Subject', D, 'DM', ''), 'BODY_TEXT': ('Note.Body', D, 'DM', ''),
  'CONFIDENTIAL_FLAG': ('Note.Confidential', X, 'DM', ''), 'AUTHOR_USER': ('Note.Author', D, 'DM', ''), 'EXPOSURE_ID': ('Note.Exposure', X, 'DM', ''),
})
t('CLM_DOCUMENT', 'CC', 'Document', 'Claim documents (DMS link).', {
  'DOC_TYPE': ('Document.Type', X, 'DM', ''), 'DOC_NAME': ('Document.Name', D, 'DM', ''), 'DOC_STATUS': ('Document.Status', X, 'DM', ''),
  'RECIPIENT_ID': ('Document.Recipient', X, 'IND', ''), 'DMS_REF': ('Document.DocUID', D, 'DM', 'External DMS id'),
  'REQUEST_DATE': ('Document.DateCreated', D, 'IND', ''), 'RECEIVED_DATE': ('Document.DateReceived', D, 'IND', ''),
})
t('CLM_SERVICE_REQUEST', 'CC', 'ServiceRequest', 'Vendor services.', {
  'VENDOR_CONTACT_ID': ('ServiceRequest.Specialist', X, 'DM', ''), 'SERVICE_TYPE': ('ServiceRequest.Kind / Services', X, 'DM', ''),
  'SR_STATUS': ('ServiceRequest.Progress', X, 'DM', ''), 'REQUEST_DATE': ('ServiceRequest.RequestedServiceCompletionDate', X, 'IND', ''),
  'QUOTE_AMOUNT': ('ServiceRequestInvoice quote', X, 'IND', ''), 'INVOICE_AMOUNT': ('ServiceRequestInvoice.Amount', X, 'DM', ''),
  'COMPLETED_DATE': ('ServiceRequest completion date', D, 'IND', ''), 'PAYMENT_TXN_ID': ('ServiceRequestInvoice payment', X, 'DM', ''),
})
t('CLM_HISTORY', 'CC', 'History', 'Claim history.', {
  'EVENT_TYPE': ('History.Type', X, 'DM', ''), 'DESCRIPTION': ('History.Description', D, 'DM', ''),
  'USER_ID': ('History.User', D, 'DM', ''), 'EVENT_TS': ('History.EventTimestamp', D, 'DM', ''), 'PROGRAM_ID': ('(not migrated)', R, '--', ''),
})
t('CLM_AUDIT_CHAIN', 'CC', 'No base equivalent (tamper-evident ledger)', 'Keep the hash chain outside Guidewire (evidence ledger) or as an extension.', {
  'PAYLOAD_DIGEST': ('Evidence ledger', G, '--', 'Archive and keep verifiable; do not migrate into CC'), 'PREV_HASH': ('Evidence ledger', G, '--', ''),
  'CHAIN_HASH': ('Evidence ledger', G, '--', ''), 'SEALED_FLAG': ('Evidence ledger', G, '--', ''),
})
t('CLM_EVENT_QUEUE', 'CC', 'Messaging (MessageDestination / Message)', 'Integration events.', {
  'PAYLOAD': ('Message.Payload', X, 'IND', ''), 'EVENT_STATUS': ('Message.Status', X, 'DM', ''),
})
t('CLM_FNOL_INTAKE', 'CC', 'FNOL Cloud API (draft claim) staging', 'Bulk FNOL intake becomes FNOL API calls.', {
  'RAW_RECORD': ('POST /claim/v1/claims payload', X, 'IND', 'Fixed 600-byte record → JSON draft claim'),
  'INTAKE_STATUS': ('integration status', R, '--', ''),
})
t('CLM_ISO_REQUEST', 'CC', 'ISO ClaimSearch integration', 'ClaimCenter has an ISO integration.', {
  'REQUEST_TYPE': ('ISO request type', X, 'IND', ''), 'ISO_STATUS': ('Claim.ISOStatus', X, 'IND', ''),
  'MATCH_COUNT': ('ISOMatchReport count', X, 'IND', ''), 'ISO_REF': ('Claim.ISOReceiveDate / ISO key', X, 'IND', ''),
})
t('CLM_ERROR_LOG', 'CC', 'Platform logging', 'Technical error log.', {})

# ======================================================= classification
TECH = {'CREATE_TS': ('CreateTime', 'derived', 'DM', 'Platform-managed'),
        'UPDATE_TS': ('UpdateTime', 'derived', 'DM', 'Platform-managed'),
        'VERSION_NO': ('BeanVersion', 'retire', '--', 'Optimistic locking handled by the platform'),
        'CREATE_USER': ('CreateUser', 'derived', 'DM', 'Platform-managed'),
        'PROCESSED_TS': ('Message processing time', 'derived', '--', ''),
        'EVENT_TS': ('event time', 'derived', '--', ''), 'ERROR_TS': ('', 'retire', '--', '')}

# table-level defaults for columns not curated individually
DEFAULTS = {
  'BIL_EVENT_QUEUE': ('retire', '--', 'Integration plumbing: routing fields are replaced by GW messaging destinations'),
  'CLM_EVENT_QUEUE': ('retire', '--', 'Integration plumbing: routing fields are replaced by GW messaging destinations'),
  'CLM_FNOL_INTAKE': ('retire', '--', 'Staging/control data of the intake job; not migrated'),
  'CLM_ISO_REQUEST': ('transform', 'IND', 'ISO integration timestamps on the claim ISO status'),
  'CLM_RI_TREATY':   ('gap', '--', 'Treaty terms live in the reinsurance system; no base ClaimCenter entity'),
  'CLM_AUDIT_CHAIN': ('gap', '--', 'Evidence ledger stays outside Guidewire'),
  'CLM_SIU_RULE':    ('transform', 'IND', 'Becomes SIU rule set definition'),
}
PLAN_REF = {'PLAN_CODE': 'parent plan reference', 'GROUP_CODE': 'Group.Name', 'LOSS_TYPE': 'ActivityPattern LOB / loss-type filter',
            'REQUEST_USER': 'CreateUser'}

def camel(c):
    return ''.join(w.capitalize() for w in c.lower().split('_'))

def classify(table_short, col, pk, fks):
    t = T.get(table_short)
    if not t:
        return None
    ent = t['entity'].split(' ')[0]
    if col in t['cols']:
        f, cls, conf, rule = t['cols'][col]
        return {'gw': f if '.' in f or ' ' in f or '(' in f else f'{ent}.{f}', 'cls': cls, 'conf': conf, 'rule': rule, 'by': 'curated'}
    if table_short in ('BIL_ERROR_LOG', 'CLM_ERROR_LOG'):
        return {'gw': 'Platform logs', 'cls': 'retire', 'conf': '--', 'rule': 'Technical log; not migrated', 'by': 'table rule'}
    if col in TECH:
        f, cls, conf, rule = TECH[col]
        return {'gw': f'{ent}.{f}' if f else '—', 'cls': cls, 'conf': conf, 'rule': rule, 'by': 'technical-column rule'}
    if col in pk and col.endswith('_ID') and len(pk) == 1:
        return {'gw': f'{ent}.PublicID', 'cls': 'transform', 'conf': 'DM', 'rule': 'Surrogate key → PublicID "' + table_short.lower() + ':<id>" for reconciliation', 'by': 'primary-key rule'}
    if col.endswith('_ID') or any(col in fk['cols'] for fk in fks):
        return {'gw': f'{ent} → {camel(col[:-3]) if col.endswith("_ID") else camel(col)} (foreign key)', 'cls': 'derived', 'conf': 'DM', 'rule': 'Relationship rebuilt by reference after parents load', 'by': 'foreign-key rule'}
    if col == 'ACTIVE_FLAG':
        return {'gw': f'{ent} active / retired flag', 'cls': 'transform', 'conf': 'DM', 'rule': 'Y/N → retired/active (admin data)', 'by': 'flag rule'}
    if table_short in DEFAULTS:
        cls, conf, rule = DEFAULTS[table_short]
        return {'gw': t['entity'], 'cls': cls, 'conf': conf, 'rule': rule, 'by': 'table default'}
    if col in PLAN_REF:
        return {'gw': f'{ent} → {PLAN_REF[col]}', 'cls': 'derived', 'conf': 'DM', 'rule': 'Reference resolved by name after parent admin data loads', 'by': 'reference rule'}
    if col.endswith('_SEQ'):
        return {'gw': '(ordering)', 'cls': 'derived', 'conf': '--', 'rule': 'Ordering column', 'by': 'sequence rule'}
    if t['cols'] == {} or ent.startswith('Platform'):
        return {'gw': t['entity'], 'cls': 'retire', 'conf': '--', 'rule': 'Platform-managed', 'by': 'table rule'}
    return {'gw': f'{ent}.{camel(col)}_Ext', 'cls': 'extension', 'conf': 'IND', 'rule': 'No curated base field; proposed extension — confirm no base field exists', 'by': 'fallback rule'}
