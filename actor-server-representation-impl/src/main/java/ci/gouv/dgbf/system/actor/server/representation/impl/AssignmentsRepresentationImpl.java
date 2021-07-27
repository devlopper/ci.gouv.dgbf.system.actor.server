package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.representation.server.AbstractSpecificRepresentationImpl;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.representation.api.AssignmentsRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AssignmentsDto;

@ApplicationScoped
public class AssignmentsRepresentationImpl extends AbstractSpecificRepresentationImpl<AssignmentsDto> implements AssignmentsRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response initialize(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(AssignmentsBusiness.class).initialize(actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response deriveAllValues(Boolean holdersSettable,Boolean assistantsSettable,Boolean overridable,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(AssignmentsBusiness.class).deriveAllValues(holdersSettable,assistantsSettable,overridable,actorCode);
					}
				};
			}
		});
	}

	@Override
	public Response applyModel(AssignmentsDto assignmentsDto, Filter.Dto filterDto, List<String> overridablesFieldsNames,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						ThrowableHelper.throwIllegalArgumentExceptionIfNull("assignmentsDto", assignmentsDto);
						ThrowableHelper.throwIllegalArgumentExceptionIfNull("filterDto", filterDto);
						Assignments assignments = MappingHelper.getDestination(assignmentsDto, Assignments.class);
						setScopeFunctionFromString(assignments, Assignments.FIELD_CREDIT_MANAGER_HOLDER, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_CREDIT_MANAGER_ASSISTANT, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_ACCOUNTING_HOLDER, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_ACCOUNTING_ASSISTANT, assignmentsDto);
						Filter filter = MappingHelper.getDestination(filterDto, Filter.class);
						return __inject__(AssignmentsBusiness.class).applyModel(assignments, filter, overridablesFieldsNames,actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response applyModelThenExport(AssignmentsDto assignmentsDto, Filter.Dto filterDto, List<String> overridablesFieldsNames,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						ThrowableHelper.throwIllegalArgumentExceptionIfNull("assignmentsDto", assignmentsDto);
						ThrowableHelper.throwIllegalArgumentExceptionIfNull("filterDto", filterDto);
						Assignments assignments = MappingHelper.getDestination(assignmentsDto, Assignments.class);
						setScopeFunctionFromString(assignments, Assignments.FIELD_CREDIT_MANAGER_HOLDER, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_CREDIT_MANAGER_ASSISTANT, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_ACCOUNTING_HOLDER, assignmentsDto);
						setScopeFunctionFromString(assignments, Assignments.FIELD_ACCOUNTING_ASSISTANT, assignmentsDto);
						Filter filter = MappingHelper.getDestination(filterDto, Filter.class);
						return __inject__(AssignmentsBusiness.class).applyModelThenExport(assignments, filter, overridablesFieldsNames,actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response applyModel(AssignmentsDto assignmentsDto,String actorCode) {
		return applyModel(assignmentsDto,assignmentsDto.getFilter(),assignmentsDto.getOverridablesFieldsNames(),actorCode);
	}
	
	@Override
	public Response applyModelThenExport(AssignmentsDto assignmentsDto,String actorCode) {
		return applyModelThenExport(assignmentsDto,assignmentsDto.getFilter(),assignmentsDto.getOverridablesFieldsNames(),actorCode);
	}
	
	@Override
	public Response saveScopeFunctions(List<AssignmentsDto> collection) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						if(CollectionHelper.isEmpty(collection))
							throw new RuntimeException("Les affectations sont obligatoire");
						Collection<Assignments> assignmentsCollection = new ArrayList<>();
						collection.forEach(index -> {
							Assignments assignments = EntityFinder.getInstance().find(Assignments.class, index.getIdentifier());
							assignments.set__auditWho__(index.get__auditWho__());
							if(assignments != null) {
								assignmentsCollection.add(assignments);
								setScopeFunctionFromString(assignments, Assignments.FIELD_CREDIT_MANAGER_HOLDER, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_CREDIT_MANAGER_ASSISTANT, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_ACCOUNTING_HOLDER, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_ACCOUNTING_ASSISTANT, index);
							}							
						});
						if(CollectionHelper.isEmpty(assignmentsCollection))
							throw new RuntimeException("Les affectations sont obligatoire");
						return __inject__(AssignmentsBusiness.class).saveScopeFunctions(assignmentsCollection);
					}
				};
			}
		});
	}
	
	@Override
	public Response saveScopeFunctionsThenExport(List<AssignmentsDto> collection) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						if(CollectionHelper.isEmpty(collection))
							throw new RuntimeException("Les affectations sont obligatoire");
						Collection<Assignments> assignmentsCollection = new ArrayList<>();
						collection.forEach(index -> {
							Assignments assignments = EntityFinder.getInstance().find(Assignments.class, index.getIdentifier());
							assignments.set__auditWho__(index.get__auditWho__());
							if(assignments != null) {
								assignmentsCollection.add(assignments);
								setScopeFunctionFromString(assignments, Assignments.FIELD_CREDIT_MANAGER_HOLDER, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_CREDIT_MANAGER_ASSISTANT, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_ACCOUNTING_HOLDER, index);
								setScopeFunctionFromString(assignments, Assignments.FIELD_ACCOUNTING_ASSISTANT, index);
							}							
						});
						if(CollectionHelper.isEmpty(assignmentsCollection))
							throw new RuntimeException("Les affectations sont obligatoire");
						return __inject__(AssignmentsBusiness.class).saveScopeFunctionsThenExport(assignmentsCollection);
					}
				};
			}
		});
	}

	@Override
	public Response clean(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(AssignmentsBusiness.class).clean(actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response import_(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(AssignmentsBusiness.class).import_(actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response importNews(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(AssignmentsBusiness.class).importNews(actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response export(String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(AssignmentsBusiness.class).export(actorCode);
					}
				};
			}
		});
	}
	
	private static void setScopeFunctionFromString(Assignments assignments,String fieldName,AssignmentsDto dto) {
		String identifier = (String)FieldHelper.read(dto, fieldName+"AsString");
		if(StringHelper.isBlank(identifier))
			FieldHelper.write(assignments, fieldName, null);
		else
			FieldHelper.write(assignments, fieldName, EntityFinder.getInstance().find(ScopeFunction.class, identifier));
	}

	public static Response importNewsAndDeriveValuesByIdentifiersAndExport(List<String> identifiers,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(AssignmentsBusiness.class).importNewsAndDeriveValuesByIdentifiersAndExport(identifiers, actorCode);
					}
				};
			}
		});
	}
}