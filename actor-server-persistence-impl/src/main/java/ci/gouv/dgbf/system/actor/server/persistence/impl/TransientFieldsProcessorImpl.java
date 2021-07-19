package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.util.Collection;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.server.hibernate.AbstractAuditIdentifiedByStringReader;
import org.cyk.utility.persistence.server.hibernate.entity.AbstractAuditIdentifiedByString;
import org.cyk.utility.persistence.server.query.ReaderByCollection;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AssignmentsAudit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionAudit;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorProfilesCodesReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorRegistrationNumberFirstNameElectronicMailAddressAdministrativeFunctionCivilityIdentityGroupAdministrativeUnitSectionReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AdministrativeUnitLocalitiesNativeReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsHoldersReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsStringsCodesNamesWithAssistantsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsStringsCodesOnlyReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsStringsCodesOnlyWithoutScopeFunctionsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeVisiblesReader;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class TransientFieldsProcessorImpl extends org.cyk.utility.persistence.server.hibernate.TransientFieldsProcessorImpl {

	@Override
	protected void __process__(Class<?> klass,Collection<?> objects,Filter filter, Collection<String> fieldsNames) {
		if(ScopeFunction.class.equals(klass))
			processScopeFunctions(CollectionHelper.cast(ScopeFunction.class, objects),fieldsNames);
		else if(Request.class.equals(klass))
			processRequests(CollectionHelper.cast(Request.class, objects),fieldsNames);
		else if(Assignments.class.equals(klass))
			processAssignments(CollectionHelper.cast(Assignments.class, objects),fieldsNames);
		else if(AdministrativeUnit.class.equals(klass))
			processAdministrativeUnits(CollectionHelper.cast(AdministrativeUnit.class, objects),fieldsNames);
		else if(Actor.class.equals(klass))
			processActors(CollectionHelper.cast(Actor.class, objects),fieldsNames);
		else if(Scope.class.equals(klass))
			processScopes(CollectionHelper.cast(Scope.class, objects),filter,fieldsNames);
		else
			super.__process__(klass,objects,filter, fieldsNames);
	}
	
	/**/
	
	@SuppressWarnings("unchecked")
	@Override
	protected <T extends AbstractAuditIdentifiedByString> AbstractAuditIdentifiedByStringReader<T> getAuditsRecordsReader(Class<T> klass) {
		if(AssignmentsAudit.class.equals(klass))
			return (AbstractAuditIdentifiedByStringReader<T>) new AssignmentsAuditReader();
		if(ScopeFunctionAudit.class.equals(klass))
			return (AbstractAuditIdentifiedByStringReader<T>) new ScopeFunctionAuditReader();
		return super.getAuditsRecordsReader(klass);
	}
	
	/**/
	public void processScopes(Collection<Scope> scopes,Filter filter,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(Scope.FIELD_VISIBLE.equals(fieldName) || Scope.FIELDS_VISIBLE_AND_VISIBLE_AS_STRING.equals(fieldName)) {
				new ScopeVisiblesReader(filter).setStringify(Scope.FIELDS_VISIBLE_AND_VISIBLE_AS_STRING.equals(fieldName)).readThenSet(scopes, null);
			}
		}
	}
	
	public void processActors(Collection<Actor> actors,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION.equals(fieldName))
				new ActorRegistrationNumberFirstNameElectronicMailAddressAdministrativeFunctionCivilityIdentityGroupAdministrativeUnitSectionReader().readThenSet(actors, null);
			else if(Actor.FIELD_PROFILES_CODES.equals(fieldName))
				new ActorProfilesCodesReader().readThenSet(actors, null);
		}
	}
	
	public void processAdministrativeUnits(Collection<AdministrativeUnit> administrativeUnits,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(AdministrativeUnit.FIELD_SUB_PREFECTURE_DEPARTMENT_REGION.equals(fieldName)) {
				AdministrativeUnitLocalitiesNativeReader administrativeUnitLocalitiesNativeReader = new AdministrativeUnitLocalitiesNativeReader();
				Collection<Object[]> arrays = administrativeUnitLocalitiesNativeReader.read(administrativeUnits, null);
				administrativeUnitLocalitiesNativeReader.set(administrativeUnits, arrays);
			}
		}
	}
	
	public void processAssignments(Collection<Assignments> assignmentsCollection,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			/*if(Assignments.FIELD___ALL__.equals(fieldName)) {
				Collection<Object[]> arrays = AssignmentsQuerier.getInstance().readAllProperties(assignmentsCollection);
				for(Assignments assignments : assignmentsCollection) {
					for(Object[] array : arrays) {
						if(array[0].equals(assignments.getIdentifier())) {
							assignments.setSectionAsString((String) array[1]);
							assignments.setAdministrativeUnitAsString((String) array[2]);
							assignments.setBudgetSpecializationUnitAsString((String) array[3]);
							assignments.setActionAsString((String) array[4]);
							assignments.setActivityAsString((String) array[5]);
							assignments.setEconomicNatureAsString((String) array[6]);
							assignments.setExpenditureNatureAsString((String) array[7]);
							assignments.setActivityCategoryAsString((String) array[8]);
							assignments.setCreditManagerHolderAsString((String) array[9]);
							assignments.setCreditManagerAssistantAsString((String) array[10]);
							assignments.setAuthorizingOfficerHolderAsString((String) array[11]);
							assignments.setAuthorizingOfficerAssistantAsString((String) array[12]);
							assignments.setFinancialControllerHolderAsString((String) array[13]);
							assignments.setFinancialControllerAssistantAsString((String) array[14]);
							assignments.setAccountingHolderAsString((String) array[15]);
							assignments.setAccountingAssistantAsString((String) array[16]);
							break;
						}
					}
				}
			}else */if(Assignments.FIELDS_ALL_STRINGS_CODES_ONLY.equals(fieldName))
				new AssignmentsStringsCodesOnlyReader().readThenSet(assignmentsCollection, null);
			else if(Assignments.FIELDS_ALL_STRINGS_CODES_NAMES_WITH_ASSISTANTS.equals(fieldName))
				new AssignmentsStringsCodesNamesWithAssistantsReader().readThenSet(assignmentsCollection, null);
			else if(Assignments.FIELDS_ALL_STRINGS_CODES_ONLY_WITHOUT_SCOPE_FUNCTIONS.equals(fieldName))
				new AssignmentsStringsCodesOnlyWithoutScopeFunctionsReader().readThenSet(assignmentsCollection, null);
			else if(Assignments.FIELDS_HOLDERS.equals(fieldName))
				new AssignmentsHoldersReader().readThenSet(assignmentsCollection, null);
			//else if(Assignments.FIELD___AUDIT_RECORDS__.equals(fieldName))
			//	new AssignmentsAuditsReader().readThenSet(assignmentsCollection, null);
		}
	}
	
	public void processScopeFunctions(Collection<ScopeFunction> scopeFunctions,Collection<String> fieldsNames) {
		Collection<Object[]> fromViewArrays = readFromViewByScopeFunctions(scopeFunctions, fieldsNames);
		Collection<String> scopeFunctionsIdentifiers = getScopeFunctionsIdentifiers(scopeFunctions, fieldsNames);
		for(String fieldName : fieldsNames) {
			if(ScopeFunction.FIELD_REQUESTED.equals(fieldName)) {
				Collection<Object[]> arrays = RequestScopeFunctionQuerier.getInstance()
						.countWhereRequestedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers(scopeFunctionsIdentifiers);								
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
						.countWhereGrantedIsTrueGroupByScopeFunctionIdentifierByScopeFunctionsIdentifiers(scopeFunctionsIdentifiers);
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
								break;
							}
						}
					}
				}				
			}else if(ScopeFunction.FIELD_ACTORS_AS_STRINGS.equals(fieldName)) {
				if(CollectionHelper.isNotEmpty(fromViewArrays)) {
					for(ScopeFunction scopeFunction : scopeFunctions) {
						scopeFunction.setActorsAsStrings(fromViewArrays.stream()
								.filter(x -> x[0].equals(scopeFunction.getCode()))
								.map(x -> x[2].toString())
								.collect(Collectors.toList()));
					}
				}				
			}else if(ScopeFunction.FIELD_ACTORS_NAMES.equals(fieldName)) {
				if(CollectionHelper.isNotEmpty(fromViewArrays)) {
					for(ScopeFunction scopeFunction : scopeFunctions) {
						scopeFunction.setActorsNames(fromViewArrays.stream()
								.filter(x -> x[0].equals(scopeFunction.getCode()))
								.map(x -> x[2].toString())
								.collect(Collectors.toList()));
					}
				}				
			}else if(ScopeFunction.FIELD_ACTORS_CODES.equals(fieldName)) {
				Collection<Object[]> arrays = ScopeFunctionQuerier.getInstance().readActorsCodes(scopeFunctions);
				if(CollectionHelper.isNotEmpty(arrays)) {
					for(ScopeFunction scopeFunction : scopeFunctions) {
						scopeFunction.setActorsCodes(arrays.stream().filter(x -> x[0].equals(scopeFunction.getIdentifier())).map(x -> x[1].toString())
								.collect(Collectors.toList()));
					}
				}
			}else if(ScopeFunction.FIELD_FUNCTION_CODE.equals(fieldName)) {
				Collection<Object[]> arrays = ScopeFunctionQuerier.getInstance().readFunctionsCodes(scopeFunctions);
				if(CollectionHelper.isNotEmpty(arrays)) {
					for(ScopeFunction scopeFunction : scopeFunctions) {
						for(Object[] array : arrays) {
							if(array[0].equals(scopeFunction.getIdentifier())) {
								scopeFunction.setFunctionCode((String)array[1]);
								break;
							}
						}
					}
				}				
			}else
				logFieldNameHasNotBeenSet(ScopeFunction.class, fieldName);
		}
	}
	
	public static Collection<String> getScopeFunctionsIdentifiers(Collection<ScopeFunction> scopeFunctions,Collection<String> fieldsNames) {
		if(CollectionUtils.containsAny(fieldsNames, ScopeFunction.FIELD_REQUESTED, ScopeFunction.FIELD_GRANTED))
			return FieldHelper.readSystemIdentifiersAsStrings(scopeFunctions);
		return null;
	}
	
	public void processRequests(Collection<Request> requests,Collection<String> fieldsNames) {		
		Collection<String> requestsIdentifiers = FieldHelper.readSystemIdentifiersAsStrings(requests);
		Collection<RequestScopeFunction> requestScopeFunctions = readRequestScopeFunctions(requestsIdentifiers, fieldsNames);		
		
		for(String fieldName : fieldsNames) {
			if(Request.FIELD_HAS_GRANTED_HOLDER_SCOPE_FUNCTION.equals(fieldName)) {
				Collection<Object[]> arrays = new ReaderByCollection.AbstractImpl<String, Object[]>() {
					protected Collection<Object[]> __read__(Collection<String> values) {
						return RequestScopeFunctionQuerier.getInstance().countWhereGrantedIsTrueGroupByRequestIdentifierByRequestsIdentifiers(values);
					}
				}.read(requestsIdentifiers);
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
			}else if(Request.FIELD_ACCOUNT_CREATION_DATE_AS_STRING.equals(fieldName)) {					
				for(Request request : requests)
					request.setAccountCreationDateAsString(TimeHelper.formatLocalDateTime(request.getAccountCreationDate()));
			}else
				logFieldNameHasNotBeenSet(ScopeFunction.class, fieldName);
		}
	}
	
	private static Collection<Object[]> readFromViewByScopeFunctions(Collection<ScopeFunction> scopeFunctions,Collection<String> fieldsNames) {
		if(CollectionUtils.containsAny(fieldsNames, ScopeFunction.FIELD_ACTORS_NAMES, ScopeFunction.FIELD_ACTORS_AS_STRINGS))
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
			return new ReaderByCollection.AbstractImpl<String, RequestScopeFunction>() {

				@Override
				protected Collection<RequestScopeFunction> __read__(Collection<String> values) {
					return RequestScopeFunctionQuerier.getInstance().readUsingScalarModeByRequestsIdentifiers(values);
				}
			}.read(requestsIdentifiers);
		return null;
	}
}