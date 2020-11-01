package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.representation.api.ExecutionImputationRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ExecutionImputationDto;

@ApplicationScoped
public class ExecutionImputationRepresentationImpl extends AbstractRepresentationEntityImpl<ExecutionImputationDto> implements ExecutionImputationRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response saveScopeFunctions(Collection<ExecutionImputationDto> executionImputationDtos) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("executionImputations", executionImputationDtos);						
						Collection<ExecutionImputation> executionImputations = new ArrayList<>();
						for(ExecutionImputationDto executionImputationDto : executionImputationDtos) {
							if(StringHelper.isBlank(executionImputationDto.getIdentifier()))
								continue;
							ExecutionImputation executionImputation = __inject__(ExecutionImputationPersistence.class).readBySystemIdentifier(executionImputationDto.getIdentifier());
							if(executionImputation == null)
								continue;
							executionImputations.add(executionImputation);
							setScopeFunctions(executionImputation,executionImputationDto);
						}
						__inject__(ExecutionImputationBusiness.class).saveScopeFunctions(executionImputations);
					}
				};
			}
		});
	}	

	@Override
	public Response deriveScopeFunctionsFromModel(ExecutionImputationDto executionImputationDto) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfNull("executionImputationDto", executionImputationDto);	
						ExecutionImputation executionImputation = MappingHelper.getDestination(executionImputationDto,ExecutionImputation.class);
						ThrowableHelper.throwIllegalArgumentExceptionIfNull("executionImputation", executionImputation);
						//setScopeFunctions(executionImputation, executionImputationDto);
						Filter filter = MappingHelper.getDestination(executionImputationDto.getFilter(), Filter.class);
						__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
					}
				};
			}
		});
	}
	
	private static void setScopeFunctions(ExecutionImputation executionImputation,ExecutionImputationDto executionImputationDto) {
		if(executionImputationDto.getCreditManager() != null) {
			executionImputation.getCreditManager(Boolean.TRUE).setHolderIdentifier(executionImputationDto.getCreditManager().getHolderIdentifier());
			executionImputation.getCreditManager(Boolean.TRUE).setAssistantIdentifier(executionImputationDto.getCreditManager().getAssistantIdentifier());
		}
		if(executionImputationDto.getAuthorizingOfficer() != null) {
			executionImputation.getAuthorizingOfficer(Boolean.TRUE).setHolderIdentifier(executionImputationDto.getAuthorizingOfficer().getHolderIdentifier());
			executionImputation.getAuthorizingOfficer(Boolean.TRUE).setAssistantIdentifier(executionImputationDto.getAuthorizingOfficer().getAssistantIdentifier());
		}
		if(executionImputationDto.getFinancialController() != null) {
			executionImputation.getFinancialController(Boolean.TRUE).setHolderIdentifier(executionImputationDto.getFinancialController().getHolderIdentifier());
			executionImputation.getFinancialController(Boolean.TRUE).setAssistantIdentifier(executionImputationDto.getFinancialController().getAssistantIdentifier());
		}
		if(executionImputationDto.getAccounting() != null) {
			executionImputation.getAccounting(Boolean.TRUE).setHolderIdentifier(executionImputationDto.getAccounting().getHolderIdentifier());
			executionImputation.getAccounting(Boolean.TRUE).setAssistantIdentifier(executionImputationDto.getAccounting().getAssistantIdentifier());
		}
	}
}