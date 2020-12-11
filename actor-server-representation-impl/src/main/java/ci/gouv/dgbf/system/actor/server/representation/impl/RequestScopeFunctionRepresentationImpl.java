package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestScopeFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestScopeFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class RequestScopeFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<RequestScopeFunctionDto> implements RequestScopeFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
