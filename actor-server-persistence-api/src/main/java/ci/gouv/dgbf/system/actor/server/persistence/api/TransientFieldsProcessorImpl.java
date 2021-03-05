package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.string.StringHelper;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class TransientFieldsProcessorImpl extends org.cyk.utility.persistence.server.TransientFieldsProcessorImpl {

	@Override
	protected void __process__(Class<?> klass,Collection<?> objects, Collection<String> fieldsNames) {
		if(ScopeFunction.class.equals(klass))
			processScopeFunctions(CollectionHelper.cast(ScopeFunction.class, objects),fieldsNames);
		else if(Request.class.equals(klass))
			processRequests(CollectionHelper.cast(Request.class, objects),fieldsNames);
		else
			super.__process__(klass,objects, fieldsNames);
	}
	
	/**/
	
	public void processScopeFunctions(Collection<ScopeFunction> scopeFunctions,Collection<String> fieldsNames) {
		Collection<Object[]> fromViewArrays = readFromViewByScopeFunctions(scopeFunctions, fieldsNames);
		//Collection<RequestScopeFunction> requestScopeFunctions = null;
		//if(fieldsNames.contains(ScopeFunction.FIELD_REQUESTED) || fieldsNames.contains(ScopeFunction.FIELD_GRANTED))
		//	requestScopeFunctions = RequestScopeFunctionQuerier.getInstance().readByScopeFunctionsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(scopeFunctions));
		for(String fieldName : fieldsNames) {
			if(ScopeFunction.FIELD_REQUESTED.equals(fieldName)) {
				Collection<Object[]> arrays = RequestScopeFunctionQuerier.getInstance()
						.countWhereRequestedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(scopeFunctions));								
				for(ScopeFunction scopeFunction : scopeFunctions) {
					for(Object[] array : arrays) {
						if(array[0].equals(scopeFunction.getIdentifier())) {
							scopeFunction.setRequested(NumberHelper.isGreaterThanZero((Long)array[1]));
							scopeFunction.setRequestedAsString(ifTrueYesElseNo(ScopeFunction.class, fieldName, scopeFunction.getRequested()));
							break;
						}
					}
				}			
			}else if(ScopeFunction.FIELD_GRANTED.equals(fieldName)) {				
				Collection<Object[]> arrays = RequestScopeFunctionQuerier.getInstance()
						.countWhereGrantedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(scopeFunctions));
				for(ScopeFunction scopeFunction : scopeFunctions) {
					for(Object[] array : arrays) {
						if(array[0].equals(scopeFunction.getIdentifier())) {
							scopeFunction.setGranted(NumberHelper.isGreaterThanZero((Long)array[1]));
							scopeFunction.setGrantedAsString(ifTrueYesElseNo(ScopeFunction.class, fieldName, scopeFunction.getGranted()));
							break;
						}
					}
				}
			}else if(ScopeFunction.FIELD_IS_HOLDER.equals(fieldName)) {				
				for(ScopeFunction scopeFunction : scopeFunctions) {
					scopeFunction.setIsHolder(StringHelper.isNotBlank(scopeFunction.getFunctionCode()) && 
						Function.EXECUTION_HOLDERS_CODES.contains(scopeFunction.getFunctionCode()));
				}
			}else if(ScopeFunction.FIELD_ACTOR_AS_STRING.equals(fieldName)) {
				if(CollectionHelper.isNotEmpty(fromViewArrays)) {
					for(ScopeFunction scopeFunction : scopeFunctions) {
						for(Object[] array : fromViewArrays) {
							if(scopeFunction.getCode().equals(array[0])) {
								scopeFunction.setActorAsString((String)array[1]);
								scopeFunction.setAssignmentToActorMessage((String)array[2]);
								break;
							}
						}
					}
				}				
			}else
				logFieldNameHasNotBeenSet(ScopeFunction.class, fieldName);
		}
	}
	
	public void processRequests(Collection<Request> requests,Collection<String> fieldsNames) {		
		Collection<String> requestsIdentifiers = FieldHelper.readSystemIdentifiersAsStrings(requests);
		Collection<RequestScopeFunction> requestScopeFunctions = readRequestScopeFunctions(requestsIdentifiers, fieldsNames);		
		
		for(String fieldName : fieldsNames) {
			if(Request.FIELD_HAS_GRANTED_HOLDER_SCOPE_FUNCTION.equals(fieldName)) {
				Collection<Object[]> arrays = RequestScopeFunctionQuerier.getInstance().countWhereGrantedIsTrueGroupByRequestIdentifierByRequestsIdentifiers(requestsIdentifiers);
				if(CollectionHelper.isNotEmpty(arrays)) {
					for(Request request : requests) {
						for(Object[] array : arrays) {
							if(request.getIdentifier().equals(array[0])) {
								request.setHasGrantedHolderScopeFunction(NumberHelper.isGreaterThanZero(NumberHelper.getLong(array[1])));
								break;
							}
						}
					}
				}
			}else if(Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_AS_STRINGS.equals(fieldName) || Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_GRANTED_AS_STRINGS.equals(fieldName)) {					
				for(Request request : requests) {
					Collection<String> strings = requestScopeFunctions.stream()
							.filter(x -> x.getRequestIdentifier().equals(request.getIdentifier()) 
									&& ( Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_GRANTED_AS_STRINGS.equals(fieldName) 
											? Boolean.TRUE.equals(x.getGranted()) : Boolean.TRUE.equals(x.getRequested())))
							.map(x -> x.getScopeFunctionCode()+" "+x.getScopeFunctionName())
							.collect(Collectors.toList());
					if(Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_AS_STRINGS.equals(fieldName))
						request.setBudgetariesScopeFunctionsAsStrings(strings);
					else if(Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_GRANTED_AS_STRINGS.equals(fieldName))
						request.setBudgetariesScopeFunctionsGrantedAsStrings(strings);
				}
			}else if(Request.FIELD_PROCESSED.equals(fieldName)) {					
				for(Request request : requests)
					request.setProcessed(request.getProcessingDate() != null);
			}else if(Request.FIELD_ACCEPTED.equals(fieldName)) {					
				for(Request request : requests)
					request.setAccepted(request.getStatus().getCode().equals(RequestStatus.CODE_ACCEPTED));
			}else if(Request.FIELD_REJECTED.equals(fieldName)) {					
				for(Request request : requests)
					request.setRejected(request.getStatus().getCode().equals(RequestStatus.CODE_REJECTED));
			}else if(Request.FIELD_IS_CREDIT_MANAGER_HOLDER.equals(fieldName)) {					
				for(Request request : requests)
					request.setIsCreditManagerHolder(hasFunctionCode(request,Function.CODE_CREDIT_MANAGER_HOLDER,requestScopeFunctions));
			}else if(Request.FIELD_IS_AUTHORIZING_OFFICER_HOLDER.equals(fieldName)) {					
				for(Request request : requests)
					request.setIsAuthorizingOfficerHolder(hasFunctionCode(request,Function.CODE_AUTHORIZING_OFFICER_HOLDER,requestScopeFunctions));
			}else if(Request.FIELD_IS_FINANCIAL_CONTROLLER_HOLDER.equals(fieldName)) {					
				for(Request request : requests)
					request.setIsFinancialControllerHolder(hasFunctionCode(request,Function.CODE_FINANCIAL_CONTROLLER_HOLDER,requestScopeFunctions));
			}else if(Request.FIELD_IS_ACCOUNTING_HOLDER.equals(fieldName)) {					
				for(Request request : requests)
					request.setIsAccountingHolder(hasFunctionCode(request,Function.CODE_ACCOUNTING_HOLDER,requestScopeFunctions));
			}else if(Request.FIELD_GRANTED_BUDGETARIES_SCOPE_FUNCTIONS.equals(fieldName)) {					
				for(Request request : requests)
					request.setGrantedBudgetariesScopeFunctions(
							requestScopeFunctions.stream()
							.filter(x -> x.getRequestIdentifier().equals(request.getIdentifier()))
							.map(x -> new ScopeFunction()
									.setIdentifier(x.getScopeFunctionIdentifier())
									.setCode(x.getScopeFunctionCode())
									.setName(x.getScopeFunctionName())
									.setFunction(StringHelper.isBlank(x.getFunctionCode()) ? null : new Function().setCode(x.getFunctionCode()))
							)
							.collect(Collectors.toList())
							);
			}else if(Request.FIELD_ACCOUNT_CREATION_MESSAGE.equals(fieldName)) {
				Collection<Object[]> arrays = RequestQuerier.getInstance().readFromViewByRequests(requests);
				for(Request request : requests)
					for(Object[] array : arrays) {
						if(request.getElectronicMailAddress().equals(array[0])) {
							request.setAccountCreationMessage((String)array[1]);
							break;
						}
					}
			}else
				logFieldNameHasNotBeenSet(ScopeFunction.class, fieldName);
		}
	}
	
	private static Collection<Object[]> readFromViewByScopeFunctions(Collection<ScopeFunction> scopeFunctions,Collection<String> fieldsNames) {
		if(CollectionUtils.containsAny(fieldsNames, ScopeFunction.FIELD_ACTOR_AS_STRING))
			return ScopeFunctionQuerier.getInstance().readFromViewByScopeFunctions(scopeFunctions);
		return null;
	}
	
	private static Boolean hasFunctionCode(Request request,String functionCode,Collection<RequestScopeFunction> requestScopeFunctions) {
		for(RequestScopeFunction requestScopeFunction : requestScopeFunctions)
			if(requestScopeFunction.getRequestIdentifier().equals(request.getIdentifier()) && functionCode.equals(requestScopeFunction.getFunctionCode()))
				return Boolean.TRUE;
		return null;
	}
	
	private static Collection<RequestScopeFunction> readRequestScopeFunctions(Collection<String> requestsIdentifiers,Collection<String> fieldsNames) {
		if(CollectionUtils.containsAny(fieldsNames, Request.FIELD_GRANTED_BUDGETARIES_SCOPE_FUNCTIONS, Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_AS_STRINGS
				,Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_GRANTED_AS_STRINGS))
			return RequestScopeFunctionQuerier.getInstance().readUsingScalarModeByRequestsIdentifiers(requestsIdentifiers);
		return null;
	}
}