package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Map;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.representation.Arguments;
import org.cyk.utility.representation.DataTransferObjectProcessor;
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
			
			if(CollectionHelper.isNotEmpty(request.getGrantedBudgetariesScopeFunctions())) {
				for(ScopeFunctionDto scopeFunction : request.getGrantedBudgetariesScopeFunctions()) {
					if(Function.CODE_CREDIT_MANAGER_HOLDER.equals(scopeFunction.getFunction().getCode())) {
						if(StringHelper.isNotBlank(request.getIdentifier()) &&  request.getType() != null && StringHelper.isNotBlank(request.getType().getCreditManagerSignatureSpecimenReportIdentifier())) {
							if(request.getStatus() != null && RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode())) {
								String uriQuery = ReportRepresentation.buildURIQuery(request.getType().getCreditManagerSignatureSpecimenReportIdentifier()
										, Map.of(REQUEST_REPORT_PARAMETER_IDENTIFIER, request.getIdentifier(),REQUEST_REPORT_PARAMETER_SCOPE_FUNCTION,scopeFunction.getIdentifier()), null, null);
								scopeFunction.setSignatureSpecimenReadReportURIQuery(uriQuery);								
								//request.setCreditManagerSignatureSpecimenReadReportURIQuery(uriQuery);
								
								/*
								if(request.getCreditManagerSignatureSpecimenReadReportURIsQueries() == null)
									request.setCreditManagerSignatureSpecimenReadReportURIsQueries(new ArrayList<>());
								request.getCreditManagerSignatureSpecimenReadReportURIsQueries().add(uriQuery);
								*/
								
							}
						}
					}else if(Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(scopeFunction.getFunction().getCode())){
						if(StringHelper.isNotBlank(request.getIdentifier()) &&  request.getType() != null && StringHelper.isNotBlank(request.getType().getAuthorizingOfficerSignatureSpecimenReportIdentifier())) {
							if(request.getStatus() != null && RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode())) {
								String uriQuery = ReportRepresentation.buildURIQuery(request.getType().getAuthorizingOfficerSignatureSpecimenReportIdentifier()
										, Map.of(REQUEST_REPORT_PARAMETER_IDENTIFIER, request.getIdentifier(),REQUEST_REPORT_PARAMETER_SCOPE_FUNCTION,scopeFunction.getIdentifier()), null, null);
								scopeFunction.setSignatureSpecimenReadReportURIQuery(uriQuery);								
								//request.setAuthorizingOfficerSignatureSpecimenReadReportURIQuery(uriQuery);
								
								/*
								if(request.getAuthorizingOfficerSignatureSpecimenReadReportURIsQueries() == null)
									request.setAuthorizingOfficerSignatureSpecimenReadReportURIsQueries(new ArrayList<>());
								request.getAuthorizingOfficerSignatureSpecimenReadReportURIsQueries().add(uriQuery);
								*/
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