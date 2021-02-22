package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.string.StringHelper;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
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
			}else
				logFieldNameHasNotBeenSet(ScopeFunction.class, fieldName);
		}
	}
	
	public void processRequests(Collection<Request> requests,Collection<String> fieldsNames) {
		Collection<RequestScopeFunction> requestScopeFunctions = null;
		Collection<String> requestsIdentifiers = FieldHelper.readSystemIdentifiersAsStrings(requests);
		if(fieldsNames.contains(Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_AS_STRINGS) || fieldsNames.contains(Request.FIELD_BUDGETARIES_SCOPE_FUNCTIONS_GRANTED_AS_STRINGS))
			requestScopeFunctions = RequestScopeFunctionQuerier.getInstance().readUsingScalarModeByRequestsIdentifiers(requestsIdentifiers);
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
			}
		}
	}
}