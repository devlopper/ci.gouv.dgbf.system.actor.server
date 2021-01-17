package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Map;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.representation.Arguments;
import org.cyk.utility.__kernel__.representation.DataTransferObjectProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.report.ReportRepresentation;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionDto;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class DataTransferObjectProcessorImpl extends DataTransferObjectProcessor.AbstractImpl implements Serializable {

	@Override
	protected <T> void __processRead__(Class<T> klass, Arguments arguments, T dto) {
		super.__processRead__(klass, arguments, dto);
		if(RequestDto.class.equals(klass)) {
			RequestDto request = (RequestDto) dto;
			if(StringHelper.isNotBlank(request.getIdentifier()) &&  request.getType() != null && StringHelper.isNotBlank(request.getType().getReportIdentifier())) {
				request.setReadReportURIQuery(ReportRepresentation.buildURIQuery(request.getType().getReportIdentifier()
						, Map.of(REQUEST_REPORT_PARAMETER_IDENTIFIER, request.getIdentifier()), null, null));
			}
			
			if(StringHelper.isNotBlank(request.getIdentifier()) &&  request.getType() != null && StringHelper.isNotBlank(request.getType().getSignatureSpecimenReportIdentifier())) {
				if(request.getStatus() != null && RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode())) {
					request.setSignatureSpecimenReadReportURIQuery(ReportRepresentation.buildURIQuery(request.getType().getSignatureSpecimenReportIdentifier()
						, Map.of(REQUEST_REPORT_PARAMETER_IDENTIFIER, request.getIdentifier()), null, null));
				}
			}
			
			if(CollectionHelper.isNotEmpty(request.getBudgetariesScopeFunctions())) {
				for(ScopeFunctionDto scopeFunction : request.getBudgetariesScopeFunctions()) {
					if(Function.CODE_CREDIT_MANAGER_HOLDER.equals(scopeFunction.getFunction().getCode())) {
						if(StringHelper.isNotBlank(request.getIdentifier()) &&  request.getType() != null && StringHelper.isNotBlank(request.getType().getCreditManagerSignatureSpecimenReportIdentifier())) {
							if(request.getStatus() != null && RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode())) {
								request.setCreditManagerSignatureSpecimenReadReportURIQuery(ReportRepresentation.buildURIQuery(request.getType().getCreditManagerSignatureSpecimenReportIdentifier()
									, Map.of(REQUEST_REPORT_PARAMETER_IDENTIFIER, request.getIdentifier(),REQUEST_REPORT_PARAMETER_SCOPE_FUNCTION,scopeFunction.getIdentifier()), null, null));
							}
						}
					}else if(Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(scopeFunction.getFunction().getCode())){
						if(StringHelper.isNotBlank(request.getIdentifier()) &&  request.getType() != null && StringHelper.isNotBlank(request.getType().getAuthorizingOfficerSignatureSpecimenReportIdentifier())) {
							if(request.getStatus() != null && RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode())) {
								request.setAuthorizingOfficerSignatureSpecimenReadReportURIQuery(ReportRepresentation.buildURIQuery(request.getType().getAuthorizingOfficerSignatureSpecimenReportIdentifier()
									, Map.of(REQUEST_REPORT_PARAMETER_IDENTIFIER, request.getIdentifier(),REQUEST_REPORT_PARAMETER_SCOPE_FUNCTION,scopeFunction.getIdentifier()), null, null));
							}
						}
					}
				}
			}
		}
	}
	
	private static final String REQUEST_REPORT_PARAMETER_IDENTIFIER = "identifiant";
	private static final String REQUEST_REPORT_PARAMETER_SCOPE_FUNCTION = "poste";
}