package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.representation.api.AssignmentsRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AssignmentsDto;

@ApplicationScoped
public class AssignmentsRepresentationImpl extends AbstractRepresentationEntityImpl<AssignmentsDto> implements AssignmentsRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
