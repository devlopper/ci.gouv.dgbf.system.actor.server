package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Map;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.log.LogHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.report.ReportRepresentation;
import org.cyk.utility.representation.Arguments;
import org.cyk.utility.representation.DataTransferObjectProcessor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetCategory;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestScopeFunctionDto;
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
			
			if(CollectionHelper.isNotEmpty(request.getGrantedBudgetariesScopeFunctions()))
				for(ScopeFunctionDto scopeFunction : request.getGrantedBudgetariesScopeFunctions())
					setSignatureSpecimenReadReportURIQuery(request, scopeFunction);
		}else if(RequestScopeFunctionDto.class.equals(klass)) {
			RequestScopeFunctionDto requestScopeFunctionDto = (RequestScopeFunctionDto) dto;
			if(StringHelper.isNotBlank(requestScopeFunctionDto.getSignatureSpecimenReportIdentifier()) && StringHelper.isNotBlank(requestScopeFunctionDto.getRequestIdentifier())
					&& StringHelper.isNotBlank(requestScopeFunctionDto.getScopeFunctionIdentifier())) {
				String uriQuery = ReportRepresentation.buildURIQuery(requestScopeFunctionDto.getSignatureSpecimenReportIdentifier(), Map.of(REQUEST_REPORT_PARAMETER_IDENTIFIER, requestScopeFunctionDto.getRequestIdentifier()
						,REQUEST_REPORT_PARAMETER_SCOPE_FUNCTION,requestScopeFunctionDto.getScopeFunctionIdentifier()), null, null);
				requestScopeFunctionDto.setSignatureSpecimenReadReportURIQuery(uriQuery);
			}
		}
	}
	
	private void setSignatureSpecimenReadReportURIQuery(RequestDto request,ScopeFunctionDto scopeFunction) {
		if(StringHelper.isBlank(request.getIdentifier()) ||  request.getType() == null || request.getStatus() == null || !RequestStatus.CODE_ACCEPTED.equals(request.getStatus().getCode()))
			return;
		String budgetCategoryCode = scopeFunction.getBudgetCategoryCode();
		if(StringHelper.isBlank(budgetCategoryCode)) {
			LogHelper.logWarning(String.format("No budget category has been found for scope function <<%s>> , code <<%s>> will be used",scopeFunction.getCode(),BudgetCategory.CODE_GENERAL), getClass());
			budgetCategoryCode = BudgetCategory.CODE_GENERAL;
		}
		String reportIdentifier = null;
		if(BudgetCategory.CODE_GENERAL.equals(budgetCategoryCode)) {
			if(Function.CODE_CREDIT_MANAGER_HOLDER.equals(scopeFunction.getFunction().getCode()))
				reportIdentifier = request.getType().getCreditManagerSignatureSpecimenReportIdentifier();
			else if(Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(scopeFunction.getFunction().getCode()))
				reportIdentifier = request.getType().getAuthorizingOfficerSignatureSpecimenReportIdentifier();
		}else if(BudgetCategory.CODE_EPN.equals(budgetCategoryCode)) {
			if(Function.CODE_CREDIT_MANAGER_HOLDER.equals(scopeFunction.getFunction().getCode()))
				reportIdentifier = request.getType().getCreditManagerSignatureSpecimenEpnReportIdentifier();
			else if(Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(scopeFunction.getFunction().getCode()))
				reportIdentifier = request.getType().getAuthorizingOfficerSignatureSpecimenEpnReportIdentifier();
		}
		if(StringHelper.isBlank(reportIdentifier)) {
			LogHelper.logWarning(String.format("No report identifier of <<%s>> has been found for scope function <<%s>>",scopeFunction.getFunction().getCode(),scopeFunction.getCode()), getClass());
			return;
		}
		scopeFunction.setSignatureSpecimenReadReportURIQuery(ReportRepresentation.buildURIQuery(reportIdentifier
			, Map.of(REQUEST_REPORT_PARAMETER_IDENTIFIER, request.getIdentifier(),REQUEST_REPORT_PARAMETER_SCOPE_FUNCTION,scopeFunction.getIdentifier()), null, null));	
	}
	
	private static final String REQUEST_REPORT_PARAMETER_IDENTIFIER = "identifiant";
	private static final String REQUEST_REPORT_PARAMETER_SCOPE_FUNCTION = "poste";
}