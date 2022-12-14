package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.server.query.executor.field.CodeExecutor;
import org.jboss.arquillian.junit.InSequence;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
import ci.gouv.dgbf.system.actor.server.business.api.RequestDispatchSlipBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class RequestBusinessIT extends AbstractBusinessInMemoryIT {

	@Inject private RequestBusiness requestBusiness;
	@Inject private RequestDispatchSlipBusiness requestDispatchSlipBusiness;
	
    @Test @InSequence(1)
    public void request_initialize() {
    	for(Integer index = 1; index <= 7; index++) {
    		String identifier = index.toString();
    		Request request = new Request().setCode(identifier).setBudgetCategoryIdentifier("1");
        	request.setType(CodeExecutor.getInstance().getOne(RequestType.class, RequestType.CODE_DEMANDE_POSTES_BUDGETAIRES)).setElectronicMailAddress("m"+identifier+"@mail.com");
        	request.setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "1")));
        	request.setAdministrativeUnit(EntityFinder.getInstance().find(AdministrativeUnit.class, "13010222"));
        	requestBusiness.initialize(request);
        	assertThatRequestStatusIsEqualTo(identifier, RequestStatus.CODE_INITIALIZED);
    	}  	
    }
    
    @Test @InSequence(2)
    public void request_submit() {
    	for(Integer index = 1; index <= 5; index++) {
    		String identifier = index.toString();
    		requestBusiness.submitByIdentifier(identifier, null);
        	assertThatRequestStatusIsEqualTo(identifier, RequestStatus.CODE_SUBMITTED);
    	}
    }
    
    @Test @InSequence(3)
    public void request_accept() {
    	for(Integer index = 1; index <= 1; index++) {
    		String identifier = index.toString();
    		requestBusiness.acceptByIdentifier(identifier, List.of("1"),null,null,null);
        	assertThatRequestStatusIsEqualTo(identifier, RequestStatus.CODE_ACCEPTED);
    	} 	
    }
    
    @Test @InSequence(4)
    public void request_reject() {
    	for(Integer index = 2; index <= 2; index++) {
    		String identifier = index.toString();
    		requestBusiness.rejectByIdentifier(identifier, "RAS", null, null);
        	assertThatRequestStatusIsEqualTo(identifier, RequestStatus.CODE_REJECTED);
    	}
    }
    
    @Test @InSequence(5)
    public void requestDispatchSlip_record() {
    	RequestDispatchSlip requestDispatchSlip = new RequestDispatchSlip().setCode("b1").setBudgetCategoryIdentifier("1")
    			.setSection(EntityFinder.getInstance().find(Section.class, "327")).setFunction(EntityFinder.getInstance().find(Function.class, "AGC"));
    	requestDispatchSlip.setRequests(List.of(
    			EntityFinder.getInstance().find(Request.class, "3").setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "1")))
    			,EntityFinder.getInstance().find(Request.class, "4").setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "1")))
    			,EntityFinder.getInstance().find(Request.class, "5").setBudgetariesScopeFunctions(List.of(EntityFinder.getInstance().find(ScopeFunction.class, "1")))
    			));
		requestDispatchSlipBusiness.record(requestDispatchSlip);
		assertThatFindByIdentifierIsNotNull(RequestDispatchSlip.class, "b1");
    	assertThat(EntityFinder.getInstance().findMany(Request.class, List.of("3","4","5")).stream().map(x -> x.getDispatchSlip().getCode()).collect(Collectors.toList()))
    		.containsExactlyInAnyOrder("b1","b1","b1");
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getCreationDate()).isNotNull();
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getSendingDate()).isNull();
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getProcessingDate()).isNull();
    }
    
    @Test @InSequence(6)
    public void requestDispatchSlip_send() {
    	requestDispatchSlipBusiness.send(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1"));
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getCreationDate()).isNotNull();
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getSendingDate()).isNotNull();
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getProcessingDate()).isNull();
    }
    
    @Test @InSequence(7)
    public void requestDispatchSlip_accept_3_reject_4_process() {
    	requestBusiness.acceptByIdentifier("3", List.of("1"),null,null,null);
    	requestBusiness.rejectByIdentifier("4", "RAS", null, null);
    	RequestDispatchSlip requestDispatchSlip = EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").setRequests(RequestQuerier.getInstance()
    			.readByDispatchSlipIdentifier("b1"));
    	CollectionHelper.getFirst(requestDispatchSlip.getRequests().stream().filter(x -> x.getIdentifier().equals("5")).collect(Collectors.toList()))
    		.setStatusAsString(RequestStatus.CODE_ACCEPTED);
    	requestDispatchSlipBusiness.process(requestDispatchSlip);
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getCreationDate()).isNotNull();
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getSendingDate()).isNotNull();
    	assertThat(EntityFinder.getInstance().find(RequestDispatchSlip.class, "b1").getProcessingDate()).isNotNull();
    }

    //@Test @InSequence(1)
    public void requestDispatchSlip_process_oneRequestIsAccepted_oneRequestIsRejected_oneRequestIsNotYetProcessed() {
    	RequestDispatchSlip requestDispatchSlip = EntityFinder.getInstance().find(RequestDispatchSlip.class, "B1");
    	assertThat(requestDispatchSlip).as("request dispatch slip with ID B1 does not exist").isNotNull();
    	requestDispatchSlipBusiness.process(requestDispatchSlip);
    }
}