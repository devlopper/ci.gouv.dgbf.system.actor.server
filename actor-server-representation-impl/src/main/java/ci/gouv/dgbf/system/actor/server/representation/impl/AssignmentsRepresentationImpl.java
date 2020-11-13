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
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.representation.api.AssignmentsRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AssignmentsDto;

@ApplicationScoped
public class AssignmentsRepresentationImpl extends AbstractRepresentationEntityImpl<AssignmentsDto> implements AssignmentsRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response initialize() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(AssignmentsBusiness.class).initialize();
					}
				};
			}
		});
	}

	@Override
	public Response applyModel(AssignmentsDto assignmentsDto, Filter.Dto filterDto, List<String> overridablesFieldsNames) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
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
						__inject__(AssignmentsBusiness.class).applyModel(assignments, filter, overridablesFieldsNames);
					}
				};
			}
		});
	}
	
	@Override
	public Response applyModel(AssignmentsDto assignmentsDto) {
		return applyModel(assignmentsDto,assignmentsDto.getFilter(),assignmentsDto.getOverridablesFieldsNames());
	}
	
	@Override
	public Response deleteAll() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(AssignmentsBusiness.class).deleteAll();
					}
				};
			}
		});
	}
	
	@Override
	public Response saveScopeFunctions(List<AssignmentsDto> collection) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						if(CollectionHelper.isEmpty(collection))
							throw new RuntimeException("Les affectations sont obligatoire");
						Collection<Assignments> assignmentsCollection = new ArrayList<>();
						collection.forEach(index -> {
							Assignments assignments = EntityFinder.getInstance().find(Assignments.class, index.getIdentifier());
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
						__inject__(AssignmentsBusiness.class).saveScopeFunctions(assignmentsCollection);
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
}