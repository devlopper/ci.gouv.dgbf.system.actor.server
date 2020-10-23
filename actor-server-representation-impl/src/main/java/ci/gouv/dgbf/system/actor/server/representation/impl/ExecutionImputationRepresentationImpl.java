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
						setScopeFunctions(executionImputation, executionImputationDto);
						Filter filter = MappingHelper.getDestination(executionImputationDto.getFilter(), Filter.class);
						__inject__(ExecutionImputationBusiness.class).deriveScopeFunctionsFromModel(executionImputation, filter);
					}
				};
			}
		});
	}
	
	private static void setScopeFunctions(ExecutionImputation executionImputation,ExecutionImputationDto executionImputationDto) {
		if(executionImputationDto.getCreditManager() != null) {
			if(executionImputationDto.getCreditManager().getHolder() != null)
				executionImputation.setCreditManagerHolderFromIdentifier(executionImputationDto.getCreditManager().getHolder().getIdentifier());
			if(executionImputationDto.getCreditManager().getAssistant() != null)
				executionImputation.setCreditManagerAssistantFromIdentifier(executionImputationDto.getCreditManager().getAssistant().getIdentifier());
		}
		if(executionImputationDto.getAuthorizingOfficer() != null) {
			if(executionImputationDto.getAuthorizingOfficer().getHolder() != null)
				executionImputation.setAuthorizingOfficerHolderFromIdentifier(executionImputationDto.getAuthorizingOfficer().getHolder().getIdentifier());
			if(executionImputationDto.getAuthorizingOfficer().getAssistant() != null)
				executionImputation.setAuthorizingOfficerAssistantFromIdentifier(executionImputationDto.getAuthorizingOfficer().getAssistant().getIdentifier());
		}
		if(executionImputationDto.getFinancialController() != null) {
			if(executionImputationDto.getFinancialController().getHolder() != null)
				executionImputation.setFinancialControllerHolderFromIdentifier(executionImputationDto.getFinancialController().getHolder().getIdentifier());
			if(executionImputationDto.getFinancialController().getAssistant() != null)
				executionImputation.setFinancialControllerAssistantFromIdentifier(executionImputationDto.getFinancialController().getAssistant().getIdentifier());
		}
		if(executionImputationDto.getAccounting() != null) {
			if(executionImputationDto.getAccounting().getHolder() != null)
				executionImputation.setAccountingHolderFromIdentifier(executionImputationDto.getAccounting().getHolder().getIdentifier());
			if(executionImputationDto.getAccounting().getAssistant() != null)
				executionImputation.setAccountingAssistantFromIdentifier(executionImputationDto.getAccounting().getAssistant().getIdentifier());
		}
	}
}