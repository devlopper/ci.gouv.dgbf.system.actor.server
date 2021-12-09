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
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AssignmentsAudit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionAudit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorCodeFirstNameLastNamesElectronicMailAddressSectionAdministrativeUnitAdministrativeFunctionReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorCodeFirstNameLastNamesReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorCodeFirstNameLastNamesSectionAdministrativeUnitAdministrativeFunctionReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorCodeFirstNameLastNamesSectionAdministrativeUnitReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorCodeNamesElectronicMailAddressReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorProfileRequestActorAsStringProfileAsStringGrantedAndGrantedAsStringReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorProfileRequestActorAsStringProfileTypeAsStringProfileAsStringGrantedAndGrantedAsStringReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorProfilesCodesReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorRegistrationNumberFirstNameLastNamesElectronicMailAddressAdministrativeFunctionCivilityIdentityGroupAdministrativeUnitSectionReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorScopeRequestActorAsStringScopeAsStringGrantedAndGrantedAsStringReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ActorScopeRequestActorAsStringScopeTypeAsStringScopeAsStringGrantedAndGrantedAsStringReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AdministrativeUnitLocalitiesNativeReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsAccountingTresorIdentifiersReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsHoldersReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsStringsCodesNamesWithAssistantsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsStringsCodesOnlyReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsStringsCodesOnlyWithoutScopeFunctionsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ProfileNumberOfActorsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ProfileRequestableAndRequestableAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ProfileTypeAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestDispatchSlipCodeReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestDispatchSlipNumberOfRequestsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestDispatchSlipSectionAsCodeFunctionAsCodeAllDatesAllNumbersOfRequestsAsStringReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestDispatchSlipSectionFunctionReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestFirstNameAndLastNamesReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestGrantedScopeFunctionsCodesIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestGrantedScopeFunctionsCodesReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestScopeFunctionIdentityScopeFunctionStringGrantedStringReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestScopeFunctionsCodesIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestScopeFunctionsCodesReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestSectionAdministrativeUnitTypeStatusCreationDateAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestSectionAdministrativeUnitTypeStatusCreationDateProcessingDateAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestSectionAsCodeAdministrativeUnitAsCodeStatusCreationDateProcessingDateAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestSectionAsCodeAdministrativeUnitAsCodeTypeStatusCreationDateAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestSectionAsCodeAdministrativeUnitTypeStatusCreationDateAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RequestSectionAsCodeAdministrativeUnitTypeStatusCreationDateProcessingDateAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeTypeAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeTypeRequestableAndRequestableAsStringsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeVisiblesReader;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class TransientFieldsProcessorImpl extends org.cyk.utility.persistence.server.hibernate.TransientFieldsProcessorImpl {

	@Override
	protected void __process__(Class<?> klass,Collection<?> objects,Filter filter, Collection<String> fieldsNames) {
		if(ScopeFunction.class.equals(klass))
			processScopeFunctions(CollectionHelper.cast(ScopeFunction.class, objects),fieldsNames);		
		else if(Assignments.class.equals(klass))
			processAssignments(CollectionHelper.cast(Assignments.class, objects),fieldsNames);
		else if(AdministrativeUnit.class.equals(klass))
			processAdministrativeUnits(CollectionHelper.cast(AdministrativeUnit.class, objects),fieldsNames);
		else if(Actor.class.equals(klass))
			processActors(CollectionHelper.cast(Actor.class, objects),fieldsNames);
		else if(ScopeType.class.equals(klass))
			processScopeTypes(CollectionHelper.cast(ScopeType.class, objects),filter,fieldsNames);
		else if(Scope.class.equals(klass))
			processScopes(CollectionHelper.cast(Scope.class, objects),filter,fieldsNames);
		else if(ActorScopeRequest.class.equals(klass))
			processActorScopeRequests(CollectionHelper.cast(ActorScopeRequest.class, objects),filter,fieldsNames);
		else if(Profile.class.equals(klass))
			processProfiles(CollectionHelper.cast(Profile.class, objects),filter,fieldsNames);
		else if(ActorProfileRequest.class.equals(klass))
			processActorProfileRequests(CollectionHelper.cast(ActorProfileRequest.class, objects),filter,fieldsNames);
		else if(Request.class.equals(klass))
			processRequests(CollectionHelper.cast(Request.class, objects),fieldsNames);
		else if(RequestScopeFunction.class.equals(klass))
			processRequestScopeFunctions(CollectionHelper.cast(RequestScopeFunction.class, objects),fieldsNames);
		else if(RequestDispatchSlip.class.equals(klass))
			processRequestDispatchSlips(CollectionHelper.cast(RequestDispatchSlip.class, objects),fieldsNames);
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
	/*public void processRequests(Collection<Request> requests,Filter filter,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS.equals(fieldName))
				new RequestSectionAdministrativeUnitStatusCreationDateProcessingDateAsStringsReader().readThenSet(requests, null);
		}
	}*/
	
	public void processProfiles(Collection<Profile> profiles,Filter filter,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(Profile.FIELD_TYPE_AS_STRING.equals(fieldName))
				new ProfileTypeAsStringsReader().readThenSet(profiles, null);
			else if(Profile.FIELD_NUMBER_OF_ACTORS.equals(fieldName))
				new ProfileNumberOfActorsReader().readThenSet(profiles, null);
			else if(Profile.FIELDS_REQUESTABLE_AND_REQUESTABLE_AS_STRING.equals(fieldName))
				new ProfileRequestableAndRequestableAsStringsReader().readThenSet(profiles, null);
		}
	}
	
	public void processScopeTypes(Collection<ScopeType> scopeTypes,Filter filter,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(ScopeType.FIELDS_REQUESTABLE_AND_REQUESTABLE_AS_STRING.equals(fieldName))
				new ScopeTypeRequestableAndRequestableAsStringsReader().readThenSet(scopeTypes, null);
		}
	}
	
	public void processScopes(Collection<Scope> scopes,Filter filter,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(Scope.FIELD_VISIBLE.equals(fieldName) || Scope.FIELDS_VISIBLE_AND_VISIBLE_AS_STRING.equals(fieldName))
				new ScopeVisiblesReader(filter).setStringify(Scope.FIELDS_VISIBLE_AND_VISIBLE_AS_STRING.equals(fieldName)).readThenSet(scopes, null);
			else if(Scope.FIELD_TYPE_AS_STRING.equals(fieldName))
				new ScopeTypeAsStringsReader().readThenSet(scopes, null);
		}
	}
	
	public void processActors(Collection<Actor> actors,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(Actor.FIELDS_REGISTRATION_NUMBER_FIRST_NAME_LAST_NAMES_ELECTRONIC_MAIL_ADDRESS_ADMINISTRATIVE_FUNCTION_CIVILITY_IDENTITY_GROUP_ADMINISTRATIVE_UNIT_SECTION.equals(fieldName))
				new ActorRegistrationNumberFirstNameLastNamesElectronicMailAddressAdministrativeFunctionCivilityIdentityGroupAdministrativeUnitSectionReader().readThenSet(actors, null);
			else if(Actor.FIELD_PROFILES_CODES.equals(fieldName))
				new ActorProfilesCodesReader().readThenSet(actors, null);
			else if(Actor.FIELDS_CODE_NAMES_ELECTRONIC_MAIL_ADDRESS.equals(fieldName))
				new ActorCodeNamesElectronicMailAddressReader().readThenSet(actors, null);
			else if(Actor.FIELDS_CODE_FIRST_NAME_LAST_NAMES.equals(fieldName))
				new ActorCodeFirstNameLastNamesReader().readThenSet(actors, null);
			else if(Actor.FIELDS_CODE_FIRST_NAME_LAST_NAMES_SECTION_ADMINISTRATIVE_UNIT.equals(fieldName))
				new ActorCodeFirstNameLastNamesSectionAdministrativeUnitReader().readThenSet(actors, null);
			else if(Actor.FIELDS_CODE_FIRST_NAME_LAST_NAMES_SECTION_ADMINISTRATIVE_UNIT_ADMINISTRATIVE_FUNCTION.equals(fieldName))
				new ActorCodeFirstNameLastNamesSectionAdministrativeUnitAdministrativeFunctionReader().readThenSet(actors, null);
			else if(Actor.FIELDS_CODE_FIRST_NAME_LAST_NAMES_ELECTRONIC_MAIL_ADDRESS_SECTION_ADMINISTRATIVE_UNIT_ADMINISTRATIVE_FUNCTION.equals(fieldName))
				new ActorCodeFirstNameLastNamesElectronicMailAddressSectionAdministrativeUnitAdministrativeFunctionReader().readThenSet(actors, null);
		}
	}
	
	public void processActorScopeRequests(Collection<ActorScopeRequest> actorScopeRequests,Filter filter,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(ActorScopeRequest.FIELDS_ACTOR_AS_STRING_SCOPE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING.equals(fieldName))
				new ActorScopeRequestActorAsStringScopeAsStringGrantedAndGrantedAsStringReader().readThenSet(actorScopeRequests, null);
			else if(ActorScopeRequest.FIELDS_ACTOR_AS_STRING_SCOPE_TYPE_AS_STRING_SCOPE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING.equals(fieldName))
				new ActorScopeRequestActorAsStringScopeTypeAsStringScopeAsStringGrantedAndGrantedAsStringReader().readThenSet(actorScopeRequests, null);
		}
	}
	
	public void processActorProfileRequests(Collection<ActorProfileRequest> actorProfileRequests,Filter filter,Collection<String> fieldsNames) {
		for(String fieldName : fieldsNames) {
			if(ActorProfileRequest.FIELDS_ACTOR_AS_STRING_PROFILE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING.equals(fieldName))
				new ActorProfileRequestActorAsStringProfileAsStringGrantedAndGrantedAsStringReader().readThenSet(actorProfileRequests, null);
			else if(ActorProfileRequest.FIELDS_ACTOR_AS_STRING_PROFILE_TYPE_AS_STRING_PROFILE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING.equals(fieldName))
				new ActorProfileRequestActorAsStringProfileTypeAsStringProfileAsStringGrantedAndGrantedAsStringReader().readThenSet(actorProfileRequests, null);
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
			else if(Assignments.FIELD_ACCOUNTING_TRESOR_IDENTIFIER.equals(fieldName))
				new AssignmentsAccountingTresorIdentifiersReader().readThenSet(assignmentsCollection, null);
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
			if(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_AS_STRINGS.equals(fieldName))
				new RequestSectionAdministrativeUnitTypeStatusCreationDateAsStringsReader().readThenSet(requests, null);
			else if(Request.FIELDS_SECTION_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS.equals(fieldName))
				new RequestSectionAdministrativeUnitTypeStatusCreationDateProcessingDateAsStringsReader().readThenSet(requests, null);
			else if(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_AS_STRINGS.equals(fieldName))
				new RequestSectionAsCodeAdministrativeUnitTypeStatusCreationDateAsStringsReader().readThenSet(requests, null);
			else if(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS.equals(fieldName))
				new RequestSectionAsCodeAdministrativeUnitTypeStatusCreationDateProcessingDateAsStringsReader().readThenSet(requests, null);
			else if(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_AS_STRINGS.equals(fieldName))
				new RequestSectionAsCodeAdministrativeUnitAsCodeTypeStatusCreationDateAsStringsReader().readThenSet(requests, null);
			else if(Request.FIELDS_SECTION_AS_CODE_ADMINISTRATIVE_UNIT_AS_CODE_TYPE_STATUS_CREATION_DATE_PROCESSING_DATE_AS_STRINGS.equals(fieldName))
				new RequestSectionAsCodeAdministrativeUnitAsCodeStatusCreationDateProcessingDateAsStringsReader().readThenSet(requests, null);
			else if(Request.FIELDS_SCOPE_FUNCTIONS_CODES.equals(fieldName))
				new RequestScopeFunctionsCodesReader().readThenSet(requests, null);
			else if(Request.FIELDS_IS_CREDIT_MANAGER_HOLDER_IS_AUTHORIZING_OFFICER_HOLDER_IS_FINANCIAL_CONTROLLER_HOLDER_IS_ACCOUNTING_HOLDER.equals(fieldName))
				new RequestIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader().readThenSet(requests, null);
			else if(Request.FIELD_DISPATCH_SLIP_CODE.equals(fieldName))
				new RequestDispatchSlipCodeReader().readThenSet(requests, null);
			else if(Request.FIELD_FIRST_NAME_AND_LAST_NAMES.equals(fieldName))
				new RequestFirstNameAndLastNamesReader().readThenSet(requests, null);
			else if(Request.FIELDS_SCOPE_FUNCTIONS_CODES_IS_CREDIT_MANAGER_HOLDER_IS_AUTHORIZING_OFFICER_HOLDER_IS_FINANCIAL_CONTROLLER_HOLDER_IS_ACCOUNTING_HOLDER.equals(fieldName))
				new RequestScopeFunctionsCodesIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader().readThenSet(requests, null);
			else if(Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES.equals(fieldName))
				new RequestGrantedScopeFunctionsCodesReader().readThenSet(requests, null);
			else if(Request.FIELDS_GRANTED_SCOPE_FUNCTIONS_CODES_IS_CREDIT_MANAGER_HOLDER_IS_AUTHORIZING_OFFICER_HOLDER_IS_FINANCIAL_CONTROLLER_HOLDER_IS_ACCOUNTING_HOLDER.equals(fieldName))
				new RequestGrantedScopeFunctionsCodesIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader().readThenSet(requests, null);
			else if(Request.FIELD_HAS_GRANTED_HOLDER_SCOPE_FUNCTION.equals(fieldName)) {
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
	
	public void processRequestScopeFunctions(Collection<RequestScopeFunction> requestScopeFunctions,Collection<String> fieldsNames) {	
		for(String fieldName : fieldsNames) {
			if(RequestScopeFunction.FIELDS_IDENTITY_SCOPE_FUNCTION_STRING_GRANTED_STRING.equals(fieldName))
				new RequestScopeFunctionIdentityScopeFunctionStringGrantedStringReader().readThenSet(requestScopeFunctions, null);
			else
				logFieldNameHasNotBeenSet(RequestDispatchSlip.class, fieldName);
		}
	}
	
	public void processRequestDispatchSlips(Collection<RequestDispatchSlip> requestDispatchSlips,Collection<String> fieldsNames) {	
		for(String fieldName : fieldsNames) {
			if(RequestDispatchSlip.FIELD_NUMBER_OF_REQUESTS.equals(fieldName))
				new RequestDispatchSlipNumberOfRequestsReader().readThenSet(requestDispatchSlips, null);
			else if(RequestDispatchSlip.FIELDS_SECTION_FUNCTION.equals(fieldName))
				new RequestDispatchSlipSectionFunctionReader().readThenSet(requestDispatchSlips, null);
			else if(RequestDispatchSlip.FIELDS_SECTION_AS_CODE_FUNCTION_AS_CODE_ALL_DATES_ALL_NUMBERS_OF_REQUESTS_AS_STRING.equals(fieldName))
				new RequestDispatchSlipSectionAsCodeFunctionAsCodeAllDatesAllNumbersOfRequestsAsStringReader().readThenSet(requestDispatchSlips, null);
			else
				logFieldNameHasNotBeenSet(RequestDispatchSlip.class, fieldName);
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