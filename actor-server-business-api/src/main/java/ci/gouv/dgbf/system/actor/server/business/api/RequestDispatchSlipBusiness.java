package ci.gouv.dgbf.system.actor.server.business.api;

import javax.transaction.Transactional;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;

public interface RequestDispatchSlipBusiness extends BusinessEntity<RequestDispatchSlip> {

	String RECORD = "RequestDispatchSlip.record";
	@Transactional
	void record(RequestDispatchSlip requestDispatchSlip);
	
	String SEND = "RequestDispatchSlip.send";
	@Transactional
	void send(RequestDispatchSlip requestDispatchSlip);
	
	String PROCESS = "RequestDispatchSlip.process";
	@Transactional
	void process(RequestDispatchSlip requestDispatchSlip);
	
	/**/
	
}